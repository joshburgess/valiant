-- | Streaming query results using server-side cursors.
--
-- Instead of fetching all rows at once, a cursor fetches rows in batches,
-- allowing processing of large result sets without loading everything into
-- memory. Must be called inside a transaction.
--
-- @
-- 'Hsqlx.Transaction.withTransaction' pool $ \\tx -> do
--   'withCursor' (txConn tx) myQuery params 100 $ \\cursor -> do
--     let loop = do
--           batch <- 'fetchBatch' cursor 100
--           unless (null batch) $ do
--             mapM_ processRow batch
--             loop
--     loop
-- @
module Hsqlx.Streaming
  ( withCursor
  , fetchBatch
  , CursorState (..)
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word64)
import Hsqlx.Connection (Connection (..), simpleQuery)
import Hsqlx.Error (HsqlxError (..), throwHsqlx)
import Hsqlx.Protocol.Backend
import Hsqlx.Protocol.Frontend
import Hsqlx.Protocol.Oid qualified as Oid
import Hsqlx.Statement (Statement (..))
import Hsqlx.Wire (recvBackendMsg, sendFrontendMsg, sendFrontendMsgs)
import System.IO.Unsafe (unsafePerformIO)

-- | State of an open cursor.
data CursorState = CursorState
  { csName :: ByteString
  -- ^ The cursor name on the server.
  , csConn :: Connection
  -- ^ The connection this cursor is open on.
  , csExhausted :: IORef Bool
  -- ^ Whether the cursor has returned all rows.
  }

-- | Open a parameterized cursor for a statement, run an action, then close.
--
-- The statement's parameters are bound via the extended query protocol
-- (Parse\/Bind), so parameterized queries work correctly.
--
-- Must be called inside a transaction.
withCursor
  :: Connection
  -> Statement p r
  -> p
  -> Int
  -- ^ Batch size hint (used as default for 'fetchBatch')
  -> (CursorState -> IO a)
  -> IO a
withCursor conn stmt params _batchSize action = do
  cursorName <- freshCursorName

  -- Prepare the statement if not already cached
  stmtName <- ensurePreparedForCursor conn stmt

  -- Declare cursor using extended query protocol:
  -- DECLARE <cursor> NO SCROLL CURSOR FOR <prepared_stmt>
  -- We use Parse to create a statement for the DECLARE, binding the
  -- original statement's parameters.
  let declareSql = "DECLARE " <> cursorName <> " NO SCROLL CURSOR FOR " <> stmtSQL stmt
      encodedParams = stmtEncode stmt params
      paramOids = V.map Oid.unOid (stmtParamOids stmt)

  -- Parse the DECLARE with param types, Bind with param values, Execute, Sync
  sendFrontendMsgs (connWire conn)
    [ Parse "" declareSql paramOids
    , Bind "" "" (V.singleton BinaryFormat) encodedParams V.empty
    , Execute "" 0
    , Sync
    ]

  -- Collect: ParseComplete, BindComplete, CommandComplete, ReadyForQuery
  waitDeclareComplete conn

  exhausted <- newIORef False
  let cs = CursorState cursorName conn exhausted
  result <- action cs

  -- Close the cursor
  _ <- simpleQuery conn ("CLOSE " <> cursorName)
  pure result

-- | Fetch the next batch of rows from a cursor.
--
-- Returns an empty list when the cursor is exhausted. Subsequent calls
-- after exhaustion return empty immediately without a round-trip.
fetchBatch :: CursorState -> Int -> IO [Vector (Maybe ByteString)]
fetchBatch cs n = do
  done <- readIORef (csExhausted cs)
  if done
    then pure []
    else do
      let fetchSql = "FETCH FORWARD " <> BS8.pack (show n) <> " FROM " <> csName cs
      sendFrontendMsg (connWire (csConn cs)) (Query fetchSql)
      rows <- collectFetchResults (csConn cs)
      if null rows
        then do
          writeIORef (csExhausted cs) True
          pure []
        else pure rows

-- Internal ------------------------------------------------------------------

ensurePreparedForCursor :: Connection -> Statement p r -> IO ByteString
ensurePreparedForCursor conn stmt = do
  -- We use the unnamed statement ("") for cursor declarations
  -- so we don't pollute the statement cache.
  -- The Parse message with "" creates/replaces the unnamed statement.
  pure ""

waitDeclareComplete :: Connection -> IO ()
waitDeclareComplete conn = go
  where
    go = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        ParseComplete -> go
        BindComplete -> go
        CommandComplete _ -> go
        ReadyForQuery status -> writeIORef (connTxStatus conn) status
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go
        other -> throwHsqlx (ProtocolError ("Unexpected in DECLARE cursor: " <> BS8.pack (show other)))

collectFetchResults :: Connection -> IO [Vector (Maybe ByteString)]
collectFetchResults conn = go []
  where
    go acc = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        RowDescription _ -> go acc
        DataRow vals -> go (vals : acc)
        CommandComplete _ -> go acc
        EmptyQueryResponse -> go acc
        ReadyForQuery status -> do
          writeIORef (connTxStatus conn) status
          pure (reverse acc)
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwHsqlx (ProtocolError ("Unexpected in cursor fetch: " <> BS8.pack (show other)))

-- | Global counter for unique cursor names.
{-# NOINLINE cursorCounter #-}
cursorCounter :: IORef Word64
cursorCounter = unsafePerformIO (newIORef 0)

freshCursorName :: IO ByteString
freshCursorName = do
  n <- atomicModifyIORef' cursorCounter (\n -> (n + 1, n))
  pure ("hsqlx_cur_" <> BS8.pack (show n))

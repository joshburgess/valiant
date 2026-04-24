{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Streaming query results using server-side cursors.
--
-- Instead of fetching all rows at once, a cursor fetches rows in batches,
-- allowing processing of large result sets without loading everything into
-- memory. Must be called inside a transaction.
--
-- Cursors require exclusive access to the connection's wire, so the entire
-- cursor lifecycle runs inside 'submitExclusive'.
--
-- @
-- 'Valiant.Transaction.withTransaction' pool $ \\tx -> do
--   'withCursor' (txConn tx) myQuery params 100 $ \\cursor -> do
--     let loop = do
--           batch <- 'fetchBatch' cursor 100
--           unless (null batch) $ do
--             mapM_ processRow batch
--             loop
--     loop
-- @
module Valiant.Streaming
  ( withCursor
  , fetchBatch
  , CursorState (..)
  ) where

import Control.Exception (SomeException, catch, onException)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word64)
import PgWire.Async (submitExclusive)
import PgWire.Connection (Connection (..))
import PgWire.Error (PgWireError (..), throwPgWire)
import PgWire.Protocol.Backend
import PgWire.Protocol.Frontend
import PgWire.Protocol.Oid qualified as Oid
import Valiant.Statement (Statement (..))
import PgWire.Wire (WireConn, recvBackendMsg, sendFrontendMsg, sendFrontendMsgs)
import System.IO.Unsafe (unsafePerformIO)

-- | State of an open cursor, valid only within the 'withCursor' callback.
-- Do not retain references to this value outside the callback scope.
data CursorState = CursorState
  { csName :: ByteString
  -- ^ The cursor name on the server.
  , csWire :: WireConn
  -- ^ Direct wire access (valid only inside exclusive mode).
  , csTxRef :: IORef TxStatus
  -- ^ Transaction status ref.
  , csExhausted :: IORef Bool
  -- ^ Whether the cursor has returned all rows.
  , csPreparedBatch :: IORef Int
  -- ^ Batch size currently baked into the unnamed prepared FETCH
  -- statement on the server. 'fetchBatch' re-parses only when the
  -- requested size differs from this value.
  }

-- | Open a parameterized cursor for a statement, run an action, then close.
--
-- The statement's parameters are bound via the extended query protocol
-- (Parse\/Bind), so parameterized queries work correctly.
--
-- Must be called inside a transaction. The entire cursor lifecycle runs
-- in exclusive mode (direct socket access).
withCursor
  :: Connection
  -> Statement p r
  -> p
  -> Int
  -- ^ Batch size hint (used as default for 'fetchBatch')
  -> (CursorState -> IO a)
  -> IO a
withCursor conn stmt params batchSize action =
  submitExclusive (connAsync conn) $ \wc txRef -> do
    cursorName <- freshCursorName

    -- Declare the cursor and prepare a FETCH statement for the hinted
    -- batch size in a single pipelined round-trip. Subsequent
    -- 'fetchBatch' calls reuse the unnamed prepared FETCH as long as
    -- the requested batch size matches.
    let declareSql = "DECLARE " <> cursorName <> " NO SCROLL CURSOR FOR " <> stmtSQL stmt
        fetchSql = "FETCH FORWARD " <> BS8.pack (show batchSize) <> " FROM " <> cursorName
        encodedParams = stmtEncode stmt params
        paramOids = V.map Oid.unOid (stmtParamOids stmt)

    sendFrontendMsgs wc
      [ Parse "" declareSql paramOids
      , Bind "" "" (V.singleton BinaryFormat) encodedParams V.empty
      , Execute "" 0
      , Parse "" fetchSql V.empty
      , Sync
      ]

    waitDeclareComplete wc txRef

    exhausted <- newIORef False
    preparedBatch <- newIORef batchSize
    let cs = CursorState cursorName wc txRef exhausted preparedBatch
        closeCursor =
          (do sendFrontendMsg wc (Query ("CLOSE " <> cursorName))
              collectSimpleDiscard wc txRef
            ) `catch` \(_ :: SomeException) -> pure ()
    result <- action cs `onException` closeCursor
    closeCursor
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
      prepared <- readIORef (csPreparedBatch cs)
      let bindExecSync =
            [ Bind "" "" V.empty V.empty (V.singleton BinaryFormat)
            , Execute "" 0
            , Sync
            ]
          msgs
            | prepared == n = bindExecSync
            | otherwise =
                let fetchSql = "FETCH FORWARD " <> BS8.pack (show n) <> " FROM " <> csName cs
                in Parse "" fetchSql V.empty : bindExecSync
      when (prepared /= n) $ writeIORef (csPreparedBatch cs) n
      sendFrontendMsgs (csWire cs) msgs
      rows <- collectFetchResults (csWire cs) (csTxRef cs)
      if null rows
        then do
          writeIORef (csExhausted cs) True
          pure []
        else pure rows

-- Internal ------------------------------------------------------------------

waitDeclareComplete :: WireConn -> IORef TxStatus -> IO ()
waitDeclareComplete wc txRef = go
  where
    go = do
      msg <- recvBackendMsg wc
      case msg of
        ParseComplete -> go
        BindComplete -> go
        CommandComplete _ -> go
        ReadyForQuery status -> writeIORef txRef status
        ErrorResponse err -> throwPgWire (QueryError err)
        NoticeResponse _ -> go
        other -> throwPgWire (ProtocolError ("Unexpected in DECLARE cursor: " <> BS8.pack (show other)))

collectFetchResults :: WireConn -> IORef TxStatus -> IO [Vector (Maybe ByteString)]
collectFetchResults wc txRef = go []
  where
    go !acc = do
      msg <- recvBackendMsg wc
      case msg of
        ParseComplete -> go acc
        BindComplete -> go acc
        RowDescription _ -> go acc
        DataRow vals -> go (vals : acc)
        CommandComplete _ -> go acc
        EmptyQueryResponse -> go acc
        ReadyForQuery status -> do
          writeIORef txRef status
          pure (reverse acc)
        ErrorResponse err -> throwPgWire (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwPgWire (ProtocolError ("Unexpected in cursor fetch: " <> BS8.pack (show other)))

-- | Discard all results from a simple query.
collectSimpleDiscard :: WireConn -> IORef TxStatus -> IO ()
collectSimpleDiscard wc txRef = go
  where
    go = do
      msg <- recvBackendMsg wc
      case msg of
        ReadyForQuery status -> writeIORef txRef status
        ErrorResponse err -> throwPgWire (QueryError err)
        _ -> go

-- | Global counter for unique cursor names.
{-# NOINLINE cursorCounter #-}
cursorCounter :: IORef Word64
cursorCounter = unsafePerformIO (newIORef 0)

freshCursorName :: IO ByteString
freshCursorName = do
  n <- atomicModifyIORef' cursorCounter (\n -> (n + 1, n))
  pure ("valiant_cur_" <> BS8.pack (show n))

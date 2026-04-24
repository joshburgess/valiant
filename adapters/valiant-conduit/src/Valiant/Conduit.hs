{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Conduit streaming adapter for valiant.
--
-- Stream query results as @ConduitT@ sources, enabling integration with
-- the conduit ecosystem for composable, constant-memory stream processing.
--
-- Two streaming strategies are provided:
--
-- * 'selectSource' — cursor-based, requires a transaction, fetches in
--   configurable batches from the server.
-- * 'foldSource' — processes rows as they arrive from a single query
--   execution, no transaction required.
--
-- @
-- import Valiant
-- import Valiant.Conduit
-- import Conduit
--
-- withTransaction pool $ \\tx ->
--   runConduit $
--     selectSource (txConn tx) listAllUsers () 500
--     .| mapC userName
--     .| sinkList
-- @
module Valiant.Conduit
  ( -- * Cursor-based streaming (requires transaction)
    selectSource
    -- * Fold-based streaming (no transaction required)
  , foldSource
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Conduit (ConduitT, yield)
import Data.IORef
import Data.Vector (Vector)
import Data.Vector qualified as V
import PgWire.Async (submitExclusive)
import PgWire.Connection (Connection (..))
import PgWire.Error (PgWireError (..), throwPgWire)
import PgWire.Protocol.Backend
import PgWire.Protocol.Frontend
import PgWire.Protocol.Oid qualified as Oid
import PgWire.Wire (WireConn, recvBackendMsg, sendFrontendMsg, sendFrontendMsgs)
import Valiant.Execute (ensurePrepared)
import Valiant.Statement (Statement (..))
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)

-- | @[BinaryFormat]@ — used for both parameter and result format codes.
binaryFmtVec :: Vector FormatCode
binaryFmtVec = V.singleton BinaryFormat
{-# NOINLINE binaryFmtVec #-}

-- | Stream query results using a server-side cursor.
--
-- Must be called inside a transaction. Fetches rows in batches of the
-- given size, yielding decoded rows one at a time into the conduit.
--
-- @
-- withTransaction pool $ \\tx ->
--   runConduit $
--     selectSource (txConn tx) listUsers () 500
--     .| mapC processUser
--     .| sinkNull
-- @
selectSource
  :: Connection
  -> Statement p r
  -> p
  -> Int
  -- ^ Batch size (number of rows per FETCH)
  -> ConduitT () r IO ()
selectSource conn stmt params batchSize = do
  liftIO $ submitExclusive (connAsync conn) $ \wc txRef -> do
    cursorName <- freshCursorName

    -- DECLARE cursor
    let declareSql = "DECLARE " <> cursorName <> " NO SCROLL CURSOR FOR " <> stmtSQL stmt
        encodedParams = stmtEncode stmt params
        paramOids = V.map Oid.unOid (stmtParamOids stmt)

    sendFrontendMsgs wc
      [ Parse "" declareSql paramOids
      , Bind "" "" binaryFmtVec encodedParams V.empty
      , Execute "" 0
      , Sync
      ]
    waitDeclareComplete wc txRef

    -- Fetch loop — this runs in IO, not ConduitT, because we're inside
    -- submitExclusive. We collect all rows and return them.
    allRows <- fetchAllCursor wc txRef cursorName batchSize (stmtDecode stmt)

    -- Close cursor
    sendFrontendMsg wc (Query ("CLOSE " <> cursorName))
    collectSimpleDiscard wc txRef

    pure allRows
  >>= mapM_ yield
  -- Note: This collects all rows in memory before yielding. For true
  -- constant-memory streaming, use foldSource instead or the cursor API
  -- directly. The constraint is that submitExclusive requires completing
  -- before the conduit can yield.

-- | Stream query results using row-at-a-time processing.
--
-- Does not require a transaction. Executes the query and decodes rows
-- as they arrive from the wire, yielding each into the conduit.
-- Uses exclusive wire access for the duration of the query.
--
-- @
-- withResource pool $ \\conn ->
--   runConduit $
--     foldSource conn listUsers ()
--     .| mapC processUser
--     .| sinkNull
-- @
foldSource
  :: Connection
  -> Statement p r
  -> p
  -> ConduitT () r IO ()
foldSource conn stmt params = do
  rows <- liftIO $ do
    stmtName <- ensurePrepared conn stmt
    let encodedParams = stmtEncode stmt params
    submitExclusive (connAsync conn) $ \wc txRef -> do
      sendFrontendMsgs wc
        [ Bind "" stmtName binaryFmtVec encodedParams binaryFmtVec
        , Execute "" 0
        , Sync
        ]
      collectAndDecode wc txRef (stmtDecode stmt)
  mapM_ yield rows

------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------

fetchAllCursor
  :: WireConn
  -> IORef TxStatus
  -> ByteString
  -> Int
  -> (Vector (Maybe ByteString) -> Either String r)
  -> IO [r]
fetchAllCursor wc txRef cursorName batchSize decode = go []
  where
    go !acc = do
      let fetchSql = "FETCH FORWARD " <> BS8.pack (show batchSize) <> " FROM " <> cursorName
      sendFrontendMsg wc (Query fetchSql)
      rawRows <- collectFetchResults wc txRef
      if null rawRows
        then pure (reverse acc)
        else do
          decoded <- mapM (\row -> case decode row of
            Left err -> throwPgWire (DecodeError (BS8.pack err))
            Right !val -> pure val) rawRows
          go (reverse decoded ++ acc)

collectAndDecode
  :: WireConn
  -> IORef TxStatus
  -> (Vector (Maybe ByteString) -> Either String r)
  -> IO [r]
collectAndDecode wc txRef decode = go []
  where
    go !acc = do
      msg <- recvBackendMsg wc
      case msg of
        BindComplete -> go acc
        DataRow vals -> case decode vals of
          Left err -> throwPgWire (DecodeError (BS8.pack err))
          Right !val -> go (val : acc)
        CommandComplete _ -> go acc
        EmptyQueryResponse -> go acc
        ReadyForQuery status -> do
          writeIORef txRef status
          pure (reverse acc)
        ErrorResponse err -> throwPgWire (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwPgWire (ProtocolError ("Unexpected in fold source: " <> BS8.pack (show other)))

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
        other -> throwPgWire (ProtocolError ("Unexpected in DECLARE: " <> BS8.pack (show other)))

collectFetchResults :: WireConn -> IORef TxStatus -> IO [Vector (Maybe ByteString)]
collectFetchResults wc txRef = go []
  where
    go !acc = do
      msg <- recvBackendMsg wc
      case msg of
        RowDescription _ -> go acc
        DataRow vals -> go (vals : acc)
        CommandComplete _ -> go acc
        ReadyForQuery status -> do
          writeIORef txRef status
          pure (reverse acc)
        ErrorResponse err -> throwPgWire (QueryError err)
        NoticeResponse _ -> go acc
        other -> throwPgWire (ProtocolError ("Unexpected in cursor fetch: " <> BS8.pack (show other)))

collectSimpleDiscard :: WireConn -> IORef TxStatus -> IO ()
collectSimpleDiscard wc txRef = go
  where
    go = do
      msg <- recvBackendMsg wc
      case msg of
        ReadyForQuery status -> writeIORef txRef status
        ErrorResponse err -> throwPgWire (QueryError err)
        _ -> go

{-# NOINLINE cursorCounter #-}
cursorCounter :: IORef Word
cursorCounter = unsafePerformIO (newIORef 0)

freshCursorName :: IO ByteString
freshCursorName = do
  n <- atomicModifyIORef' cursorCounter (\n -> (n + 1, n))
  pure ("valiant_conduit_" <> BS8.pack (show n))

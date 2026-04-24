{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Streamly streaming adapter for valiant.
--
-- Produces @Stream IO r@ values from query results, enabling
-- high-performance composable stream processing with streamly.
--
-- @
-- import Valiant
-- import Valiant.Streamly
-- import Streamly.Data.Stream qualified as Stream
--
-- withTransaction pool $ \\tx -> do
--   count <- Stream.fold Fold.length $
--     selectStreamly (txConn tx) listAllUsers () 500
--   print count
-- @
module Valiant.Streamly
  ( -- * Cursor-based streaming (requires transaction)
    selectStreamly
    -- * Fold-based streaming (no transaction required)
  , foldStreamly
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
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
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import System.IO.Unsafe (unsafePerformIO)

binaryFmtVec :: Vector FormatCode
binaryFmtVec = V.singleton BinaryFormat
{-# NOINLINE binaryFmtVec #-}

-- | Stream query results using a server-side cursor.
--
-- Must be called inside a transaction. Fetches rows in batches and
-- yields decoded rows one at a time.
selectStreamly
  :: Connection
  -> Statement p r
  -> p
  -> Int
  -- ^ Batch size
  -> Stream IO r
selectStreamly conn stmt params batchSize =
  Stream.concatEffect $ do
    rows <- submitExclusive (connAsync conn) $ \wc txRef -> do
      cursorName <- freshCursorName
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
      allRows <- fetchAllCursor wc txRef cursorName batchSize (stmtDecode stmt)
      sendFrontendMsg wc (Query ("CLOSE " <> cursorName))
      collectSimpleDiscard wc txRef
      pure allRows
    pure (Stream.fromList rows)

-- | Stream query results using row-at-a-time processing.
--
-- Does not require a transaction.
foldStreamly
  :: Connection
  -> Statement p r
  -> p
  -> Stream IO r
foldStreamly conn stmt params =
  Stream.concatEffect $ do
    rows <- do
      stmtName <- ensurePrepared conn stmt
      let encodedParams = stmtEncode stmt params
      submitExclusive (connAsync conn) $ \wc txRef -> do
        sendFrontendMsgs wc
          [ Bind "" stmtName binaryFmtVec encodedParams binaryFmtVec
          , Execute "" 0
          , Sync
          ]
        collectAndDecode wc txRef (stmtDecode stmt)
    pure (Stream.fromList rows)

------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------

fetchAllCursor
  :: WireConn -> IORef TxStatus -> ByteString -> Int
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
  :: WireConn -> IORef TxStatus
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
        other -> throwPgWire (ProtocolError ("Unexpected in fold stream: " <> BS8.pack (show other)))

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
  pure ("valiant_streamly_" <> BS8.pack (show n))

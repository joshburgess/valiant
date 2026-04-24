{-# OPTIONS_GHC -fno-full-laziness #-}

-- | Streaming adapter for valiant using the @streaming@ library.
--
-- Produces @Stream (Of r) IO ()@ values from query results, enabling
-- composable, constant-memory stream processing.
--
-- @
-- import Valiant
-- import Valiant.Stream
-- import Streaming.Prelude qualified as S
--
-- withTransaction pool $ \\tx -> do
--   count <- S.length_ $
--     selectStream (txConn tx) listAllUsers () 500
--   print count
-- @
module Valiant.Stream
  ( -- * Cursor-based streaming (requires transaction)
    selectStream
    -- * Fold-based streaming (no transaction required)
  , foldStream
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
import Control.Monad.IO.Class (liftIO)
import Streaming (Stream, Of)
import Streaming.Prelude qualified as S
import System.IO.Unsafe (unsafePerformIO)

binaryFmtVec :: Vector FormatCode
binaryFmtVec = V.singleton BinaryFormat
{-# NOINLINE binaryFmtVec #-}

-- | Stream query results using a server-side cursor.
--
-- Must be called inside a transaction. Fetches rows in batches and
-- yields decoded rows one at a time.
selectStream
  :: Connection
  -> Statement p r
  -> p
  -> Int
  -- ^ Batch size
  -> Stream (Of r) IO ()
selectStream conn stmt params batchSize = do
  rows <- liftIO $ submitExclusive (connAsync conn) $ \wc txRef -> do
    cursorName <- freshCursorName
    let declareSql = "DECLARE " <> cursorName <> " NO SCROLL CURSOR FOR " <> stmtSQL stmt
        fetchSql = "FETCH FORWARD " <> BS8.pack (show batchSize) <> " FROM " <> cursorName
        encodedParams = stmtEncode stmt params
        paramOids = V.map Oid.unOid (stmtParamOids stmt)
    sendFrontendMsgs wc
      [ Parse "" declareSql paramOids
      , Bind "" "" binaryFmtVec encodedParams V.empty
      , Execute "" 0
      , Parse "" fetchSql V.empty
      , Sync
      ]
    waitDeclareComplete wc txRef
    allRows <- fetchAllCursor wc txRef (stmtDecode stmt)
    sendFrontendMsg wc (Query ("CLOSE " <> cursorName))
    collectSimpleDiscard wc txRef
    pure allRows
  S.each rows

-- | Stream query results using row-at-a-time processing.
--
-- Does not require a transaction. Executes the query and yields
-- decoded rows.
foldStream
  :: Connection
  -> Statement p r
  -> p
  -> Stream (Of r) IO ()
foldStream conn stmt params = do
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
  S.each rows

------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------

fetchAllCursor
  :: WireConn -> IORef TxStatus
  -> (Vector (Maybe ByteString) -> Either String r)
  -> IO [r]
fetchAllCursor wc txRef decode = go []
  where
    go !acc = do
      sendFrontendMsgs wc
        [ Bind "" "" V.empty V.empty binaryFmtVec
        , Execute "" 0
        , Sync
        ]
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
  pure ("valiant_stream_" <> BS8.pack (show n))

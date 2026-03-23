{-# OPTIONS_GHC -fno-full-laziness #-}

-- | COPY protocol support for bulk data import/export.
--
-- COPY operations require exclusive access to the connection's wire,
-- so they use 'submitExclusive' to pause the async pipeline.
module Hsqlx.Copy
  ( copyIn
  , copyOut
  , copyInBinary
  , CopyResult (..)
  ) where

import Control.Exception (SomeException, catch, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Int (Int16, Int64)
import Data.Vector (Vector)
import Data.Vector qualified as V
import PgWire.Async (submitExclusive)
import PgWire.Connection (Connection (..))
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Backend
import PgWire.Protocol.Frontend
import PgWire.Wire (WireConn, recvBackendMsg, sendFrontendMsg)

-- | Result of a COPY operation, containing the number of rows transferred.
data CopyResult = CopyResult
  { copyRows :: Int64
  -- ^ Number of rows copied, as reported by the server's @CommandComplete@ tag.
  }
  deriving stock (Show, Eq)

-- | Execute a @COPY ... FROM STDIN@ command, sending data in chunks.
--
-- Usage:
--
-- > copyIn conn "COPY users (name, email) FROM STDIN WITH (FORMAT csv)" $ \sendChunk -> do
-- >   sendChunk "Alice,alice@example.com\n"
-- >   sendChunk "Bob,bob@example.com\n"
copyIn :: Connection -> ByteString -> ((ByteString -> IO ()) -> IO ()) -> IO CopyResult
copyIn conn sql producer =
  submitExclusive (connAsync conn) $ \wc txRef -> do
    sendFrontendMsg wc (Query sql)
    waitCopyIn wc
    producer (\chunk -> sendFrontendMsg wc (CopyData chunk))
      `catch` \(e :: SomeException) -> do
        -- Best-effort: send CopyFail and drain to ReadyForQuery so the
        -- connection is usable. If the wire is dead, ignore the cleanup
        -- failure and re-throw the original exception.
        (do sendFrontendMsg wc (CopyFail (BS8.pack (show e)))
            drainToReady wc txRef
          ) `catch` \(_ :: SomeException) -> pure ()
        throwIO e
    sendFrontendMsg wc CopyDone
    collectCopyResult wc txRef

-- | Execute a @COPY ... TO STDOUT@ command, receiving data in chunks
-- via a callback.
--
-- Usage:
--
-- > copyOut conn "COPY users TO STDOUT WITH (FORMAT csv)" $ \chunk -> do
-- >   BS.putStr chunk
copyOut :: Connection -> ByteString -> (ByteString -> IO ()) -> IO CopyResult
copyOut conn sql consumer =
  submitExclusive (connAsync conn) $ \wc txRef -> do
    sendFrontendMsg wc (Query sql)
    waitCopyOut wc
    let loop = do
          msg <- recvBackendMsg wc
          case msg of
            CopyDataMsg chunk -> do
              consumer chunk
              loop
            CopyDoneMsg -> pure ()
            ErrorResponse err -> throwHsqlx (QueryError err)
            other -> throwHsqlx (ProtocolError ("Unexpected in COPY OUT: " <> BS8.pack (show other)))
    loop
    collectCopyResult wc txRef

-- | Execute a @COPY ... FROM STDIN WITH (FORMAT binary)@ command,
-- sending rows in PostgreSQL's binary COPY format.
--
-- Each row is provided as a @Vector (Maybe ByteString)@ where each
-- element is an already-encoded binary value (using 'pgEncode'), or
-- 'Nothing' for NULL.
--
-- @
-- copyInBinary conn
--   \"COPY users (id, name) FROM STDIN WITH (FORMAT binary)\"
--   2  -- number of columns
--   $ \\sendRow -> do
--     sendRow (V.fromList [Just (pgEncode (1 :: Int32)), Just (pgEncode (\"Alice\" :: Text))])
--     sendRow (V.fromList [Just (pgEncode (2 :: Int32)), Just (pgEncode (\"Bob\" :: Text))])
-- @
copyInBinary
  :: Connection
  -> ByteString
  -- ^ COPY ... FROM STDIN WITH (FORMAT binary) statement
  -> Int16
  -- ^ Number of columns
  -> ((Vector (Maybe ByteString) -> IO ()) -> IO ())
  -- ^ Producer: call @sendRow@ for each row
  -> IO CopyResult
copyInBinary conn sql numCols producer =
  submitExclusive (connAsync conn) $ \wc txRef -> do
    sendFrontendMsg wc (Query sql)
    waitCopyIn wc
    sendFrontendMsg wc (CopyData binaryCopyHeader)
    let sendRow row = do
          let rowBytes = encodeBinaryRow numCols row
          sendFrontendMsg wc (CopyData rowBytes)
    producer sendRow
      `catch` \(e :: SomeException) -> do
        (do sendFrontendMsg wc (CopyFail (BS8.pack (show e)))
            drainToReady wc txRef
          ) `catch` \(_ :: SomeException) -> pure ()
        throwIO e
    sendFrontendMsg wc (CopyData binaryCopyTrailer)
    sendFrontendMsg wc CopyDone
    collectCopyResult wc txRef

-- | Binary COPY header:
-- 11-byte signature: "PGCOPY\n\377\r\n\0"
-- 4-byte flags: 0 (no OID inclusion)
-- 4-byte header extension area length: 0
binaryCopyHeader :: ByteString
binaryCopyHeader = LBS.toStrict . B.toLazyByteString $
  B.byteString "PGCOPY\n\xff\r\n\0"
    <> B.int32BE 0  -- flags
    <> B.int32BE 0  -- header extension length

-- | Binary COPY trailer: -1 as Int16
binaryCopyTrailer :: ByteString
binaryCopyTrailer = LBS.toStrict . B.toLazyByteString $
  B.int16BE (-1)

-- | Encode a single row in binary COPY format:
-- Int16 field count, then per field: Int32 length (-1 for NULL) + data
encodeBinaryRow :: Int16 -> Vector (Maybe ByteString) -> ByteString
encodeBinaryRow numCols row = LBS.toStrict . B.toLazyByteString $
  B.int16BE numCols
    <> V.foldl' (\acc mv -> acc <> encodeField mv) mempty row
  where
    encodeField Nothing = B.int32BE (-1)
    encodeField (Just bs) = B.int32BE (fromIntegral (BS.length bs)) <> B.byteString bs

-- Internal ------------------------------------------------------------------

waitCopyIn :: WireConn -> IO ()
waitCopyIn wc = do
  msg <- recvBackendMsg wc
  case msg of
    CopyInResponse _ _ -> pure ()
    ErrorResponse err -> throwHsqlx (QueryError err)
    other -> throwHsqlx (ProtocolError ("Expected CopyInResponse, got: " <> BS8.pack (show other)))

waitCopyOut :: WireConn -> IO ()
waitCopyOut wc = do
  msg <- recvBackendMsg wc
  case msg of
    CopyOutResponse _ _ -> pure ()
    ErrorResponse err -> throwHsqlx (QueryError err)
    other -> throwHsqlx (ProtocolError ("Expected CopyOutResponse, got: " <> BS8.pack (show other)))

collectCopyResult :: WireConn -> IORef TxStatus -> IO CopyResult
collectCopyResult wc txRef = go 0
  where
    go !n = do
      msg <- recvBackendMsg wc
      case msg of
        CommandComplete tag -> go (tagRows tag)
        ReadyForQuery status -> do
          writeIORef txRef status
          pure (CopyResult n)
        ErrorResponse err -> throwHsqlx (QueryError err)
        NoticeResponse _ -> go n
        other -> throwHsqlx (ProtocolError ("Unexpected in COPY result: " <> BS8.pack (show other)))

    tagRows (InsertTag r) = r
    tagRows (UpdateTag r) = r
    tagRows (DeleteTag r) = r
    tagRows (SelectTag r) = r
    tagRows (OtherTag t) =
      let parts = BS8.words t
       in case parts of
            [_, numStr] -> maybe 0 (fromIntegral . fst) (BS8.readInt numStr)
            _ -> 0

-- | Drain messages until ReadyForQuery, ignoring errors.
-- Used after CopyFail to ensure the connection is in a clean state.
drainToReady :: WireConn -> IORef TxStatus -> IO ()
drainToReady wc txRef = go
  where
    go = do
      msg <- recvBackendMsg wc
      case msg of
        ReadyForQuery status -> writeIORef txRef status
        _ -> go

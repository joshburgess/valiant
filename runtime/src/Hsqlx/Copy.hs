{-# OPTIONS_GHC -fno-full-laziness #-}

-- | COPY protocol support for bulk data import/export.
module Hsqlx.Copy
  ( copyIn
  , copyOut
  , copyInBinary
  , CopyResult (..)
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Int (Int16, Int32, Int64)
import Data.Vector (Vector)
import Data.Vector qualified as V
import PgWire.Connection (Connection (..))
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Backend
import PgWire.Protocol.Frontend
import PgWire.Wire (recvBackendMsg, sendFrontendMsg)

-- | Result of a COPY IN operation.
data CopyResult = CopyResult
  { copyRows :: Int64
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
copyIn conn sql producer = do
  -- Send the COPY command via simple query
  sendFrontendMsg (connWire conn) (Query sql)

  -- Wait for CopyInResponse
  waitCopyIn conn

  -- Send data chunks
  producer (\chunk -> sendFrontendMsg (connWire conn) (CopyData chunk))

  -- Signal end of data
  sendFrontendMsg (connWire conn) CopyDone

  -- Collect result
  collectCopyResult conn

-- | Execute a @COPY ... TO STDOUT@ command, receiving data in chunks
-- via a callback.
--
-- Usage:
--
-- > copyOut conn "COPY users TO STDOUT WITH (FORMAT csv)" $ \chunk -> do
-- >   BS.putStr chunk
copyOut :: Connection -> ByteString -> (ByteString -> IO ()) -> IO CopyResult
copyOut conn sql consumer = do
  sendFrontendMsg (connWire conn) (Query sql)

  -- Wait for CopyOutResponse
  waitCopyOut conn

  -- Receive data chunks until CopyDone
  let loop = do
        msg <- recvBackendMsg (connWire conn)
        case msg of
          CopyDataMsg chunk -> do
            consumer chunk
            loop
          CopyDoneMsg -> pure ()
          ErrorResponse err -> throwHsqlx (QueryError err)
          other -> throwHsqlx (ProtocolError ("Unexpected in COPY OUT: " <> BS8.pack (show other)))
  loop

  collectCopyResult conn

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
copyInBinary conn sql numCols producer = do
  sendFrontendMsg (connWire conn) (Query sql)
  waitCopyIn conn

  -- Send binary COPY header
  sendFrontendMsg (connWire conn) (CopyData binaryCopyHeader)

  -- Send rows via producer
  producer $ \row -> do
    let rowBytes = encodeBinaryRow numCols row
    sendFrontendMsg (connWire conn) (CopyData rowBytes)

  -- Send binary COPY trailer + CopyDone
  sendFrontendMsg (connWire conn) (CopyData binaryCopyTrailer)
  sendFrontendMsg (connWire conn) CopyDone

  collectCopyResult conn

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

waitCopyIn :: Connection -> IO ()
waitCopyIn conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    CopyInResponse _ _ -> pure ()
    ErrorResponse err -> throwHsqlx (QueryError err)
    other -> throwHsqlx (ProtocolError ("Expected CopyInResponse, got: " <> BS8.pack (show other)))

waitCopyOut :: Connection -> IO ()
waitCopyOut conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    CopyOutResponse _ _ -> pure ()
    ErrorResponse err -> throwHsqlx (QueryError err)
    other -> throwHsqlx (ProtocolError ("Expected CopyOutResponse, got: " <> BS8.pack (show other)))

collectCopyResult :: Connection -> IO CopyResult
collectCopyResult conn = go 0
  where
    go !n = do
      msg <- recvBackendMsg (connWire conn)
      case msg of
        CommandComplete tag -> go (tagRows tag)
        ReadyForQuery status -> do
          writeIORef (connTxStatus conn) status
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

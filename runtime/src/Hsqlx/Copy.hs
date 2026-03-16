{-# OPTIONS_GHC -fno-full-laziness #-}

-- | COPY protocol support for bulk data import/export.
module Hsqlx.Copy
  ( copyIn
  , copyOut
  , CopyResult (..)
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Int (Int64)
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
    go n = do
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

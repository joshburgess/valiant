-- | A minimal mock PostgreSQL server for testing.
--
-- Speaks enough of the PG v3 wire protocol to test client behavior
-- without a real database: authentication, simple queries, extended
-- query protocol, error responses, and connection lifecycle.
--
-- @
-- withMockServer defaultMockConfig $ \\port -> do
--   conn <- connectString (\"postgres:\/\/user:pass\@localhost:\" <> BS8.pack (show port) <> \"\/testdb\")
--   (rows, _) <- simpleQuery conn \"SELECT 1\"
--   close conn
-- @
module PgWire.MockServer
  ( -- * Server lifecycle
    withMockServer
  , MockConfig (..)
  , defaultMockConfig
    -- * Query handlers
  , QueryHandler
  , simpleHandler
  , errorHandler
  , failNTimes
    -- * Response builders
  , sendBackendMsg
  , buildBackendMsg
  , buildErrorResponse
  , buildCommandComplete
  , buildRowDescription
  , buildDataRow
  , buildReadyForQuery
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (SomeException, catch, finally)
import Data.IORef
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32)
import Data.Word (Word8)
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB

-- | Configuration for the mock server.
data MockConfig = MockConfig
  { mockUser :: ByteString
  -- ^ Expected username. Default: @\"testuser\"@.
  , mockPassword :: ByteString
  -- ^ Expected password. Default: @\"testpass\"@.
  , mockDatabase :: ByteString
  -- ^ Expected database name. Default: @\"testdb\"@.
  , mockQueryHandler :: QueryHandler
  -- ^ Handler for simple queries. Default: returns empty result.
  }

-- | Handler called for each simple query. Receives the SQL text and
-- a function to send backend messages.
type QueryHandler = ByteString -> (Builder -> IO ()) -> IO ()

-- | Default config: accepts auth, returns empty results for all queries.
defaultMockConfig :: MockConfig
defaultMockConfig = MockConfig
  { mockUser = "testuser"
  , mockPassword = "testpass"
  , mockDatabase = "testdb"
  , mockQueryHandler = \_ send -> do
      send $ buildCommandComplete "SELECT 0"
  }

-- | A simple query handler that returns fixed rows for a specific query.
simpleHandler :: [(ByteString, [[(ByteString, ByteString)]])] -> QueryHandler
simpleHandler table sql send = case lookup sql table of
  Nothing -> send $ buildCommandComplete "SELECT 0"
  Just rows -> do
    case rows of
      [] -> send $ buildCommandComplete "SELECT 0"
      (first : _) -> do
        -- Send RowDescription
        let cols = map fst first
        send $ buildRowDescription cols
        -- Send DataRows
        mapM_ (\row -> send $ buildDataRow (map snd row)) rows
        send $ buildCommandComplete ("SELECT " <> BS8.pack (show (length rows)))

-- | A handler that returns a PG error with the given SQLSTATE and message.
--
-- @
-- errorHandler \"23505\" \"duplicate key value violates unique constraint \\\"users_email_key\\\"\"
-- @
errorHandler :: ByteString -> ByteString -> QueryHandler
errorHandler sqlstate msg _ send =
  send $ buildErrorResponse sqlstate msg

-- | A handler backed by an IORef counter. Returns an error for the first
-- @n@ calls, then delegates to the fallback handler. Useful for testing
-- retry logic.
--
-- @
-- ref <- newIORef 0
-- let handler = failNTimes ref 2 \"40001\" \"serialization failure\" defaultHandler
-- @
failNTimes :: IORef Int -> Int -> ByteString -> ByteString -> QueryHandler -> QueryHandler
failNTimes ref n sqlstate msg fallback sql send = do
  count <- atomicModifyIORef' ref (\c -> (c + 1, c))
  if count < n
    then send $ buildErrorResponse sqlstate msg
    else fallback sql send

-- | Start a mock server on a random port, run an action with the port,
-- then shut down.
withMockServer :: MockConfig -> (NS.PortNumber -> IO a) -> IO a
withMockServer cfg action = do
  -- Create a listening socket
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.bind sock (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
  NS.listen sock 5
  port <- NS.socketPort sock

  -- Start acceptor thread
  tid <- forkIO $ acceptLoop sock cfg

  -- Run the action, then clean up
  action port `finally` do
    killThread tid
    NS.close sock

-- | Accept loop: handle one connection at a time.
acceptLoop :: NS.Socket -> MockConfig -> IO ()
acceptLoop listenSock cfg = go
  where
    go = do
      (clientSock, _) <- NS.accept listenSock
      _ <- forkIO (handleClient clientSock cfg `catch` \(_ :: SomeException) -> pure ())
      go

-- | Handle a single client connection.
handleClient :: NS.Socket -> MockConfig -> IO ()
handleClient sock cfg = do
  -- Read startup message (no tag byte)
  startupBytes <- recvN sock 4
  let startupLen = decodeInt32BE startupBytes 0
  _payload <- recvN sock (fromIntegral startupLen - 4)

  -- Send AuthOk (cleartext auth skipped for simplicity)
  sendBuilder sock $ buildAuth 0 -- AuthOk

  -- Send server parameters
  sendBuilder sock $ buildParameterStatus "server_version" "16.0"
  sendBuilder sock $ buildParameterStatus "server_encoding" "UTF8"
  sendBuilder sock $ buildParameterStatus "client_encoding" "UTF8"
  sendBuilder sock $ buildParameterStatus "is_superuser" "off"

  -- Send BackendKeyData
  sendBuilder sock $ buildBackendKeyData 1234 5678

  -- Send ReadyForQuery (Idle)
  sendBuilder sock $ buildReadyForQuery 'I'

  -- Message loop
  let loop = do
        mTag <- recvMaybe sock 1
        case mTag of
          Nothing -> pure () -- client disconnected
          Just tagBs -> do
            let tag = BS.index tagBs 0
            lenBs <- recvN sock 4
            let len = decodeInt32BE lenBs 0
            payload <- if len > 4 then recvN sock (fromIntegral len - 4) else pure BS.empty
            handleMessage sock cfg tag payload
            loop

  loop `catch` \(_ :: SomeException) -> pure ()
  NS.close sock

handleMessage :: NS.Socket -> MockConfig -> Word8 -> ByteString -> IO ()
handleMessage sock cfg tag payload = case tag of
  -- 'Q' = Simple Query
  0x51 -> do
    let sql = BS.takeWhile (/= 0) payload -- strip NUL terminator
    mockQueryHandler cfg sql (sendBuilder sock)
    sendBuilder sock $ buildReadyForQuery 'I'

  -- 'P' = Parse
  0x50 -> do
    sendBuilder sock $ buildSimpleTag '1' -- ParseComplete

  -- 'B' = Bind
  0x42 -> do
    sendBuilder sock $ buildSimpleTag '2' -- BindComplete

  -- 'D' = Describe
  0x44 -> do
    sendBuilder sock $ buildNoData

  -- 'E' = Execute
  0x45 -> do
    sendBuilder sock $ buildCommandComplete "SELECT 0"

  -- 'S' = Sync
  0x53 -> do
    sendBuilder sock $ buildReadyForQuery 'I'

  -- 'H' = Flush
  0x48 -> pure ()

  -- 'C' = Close
  0x43 -> do
    sendBuilder sock $ buildSimpleTag '3' -- CloseComplete

  -- 'X' = Terminate
  0x58 -> pure () -- connection will close

  -- Unknown
  _ -> do
    sendBuilder sock $ buildErrorResponse "08P01" ("Unknown message tag: " <> BS8.pack (show tag))
    sendBuilder sock $ buildReadyForQuery 'E'

------------------------------------------------------------------------
-- Backend message builders
------------------------------------------------------------------------

-- | Build a complete backend message as a Builder.
buildBackendMsg :: Word8 -> Builder -> Builder
buildBackendMsg tag payload =
  let payloadBs = LBS.toStrict (B.toLazyByteString payload)
      len = fromIntegral (BS.length payloadBs + 4) :: Int32
   in B.word8 tag <> B.int32BE len <> B.byteString payloadBs

-- | Send a backend message builder to a socket.
sendBackendMsg :: NS.Socket -> Word8 -> Builder -> IO ()
sendBackendMsg sock tag payload = sendBuilder sock (buildBackendMsg tag payload)

buildAuth :: Int32 -> Builder
buildAuth authType = buildBackendMsg 0x52 (B.int32BE authType)

buildParameterStatus :: ByteString -> ByteString -> Builder
buildParameterStatus name value =
  buildBackendMsg 0x53 (B.byteString name <> B.word8 0 <> B.byteString value <> B.word8 0)

buildBackendKeyData :: Int32 -> Int32 -> Builder
buildBackendKeyData pid key =
  buildBackendMsg 0x4B (B.int32BE pid <> B.int32BE key)

buildReadyForQuery :: Char -> Builder
buildReadyForQuery status =
  buildBackendMsg 0x5A (B.word8 (fromIntegral (fromEnum status)))

buildSimpleTag :: Char -> Builder
buildSimpleTag c = buildBackendMsg (fromIntegral (fromEnum c)) mempty

buildNoData :: Builder
buildNoData = buildSimpleTag 'n'

buildCommandComplete :: ByteString -> Builder
buildCommandComplete tag =
  buildBackendMsg 0x43 (B.byteString tag <> B.word8 0)

buildRowDescription :: [ByteString] -> Builder
buildRowDescription cols =
  buildBackendMsg 0x54 $ do
    B.int16BE (fromIntegral (length cols))
    <> foldMap buildFieldInfo cols
  where
    buildFieldInfo name =
      B.byteString name <> B.word8 0  -- name + NUL
        <> B.word32BE 0               -- table OID
        <> B.int16BE 0                -- column number
        <> B.word32BE 25              -- type OID (text)
        <> B.int16BE (-1)             -- type size
        <> B.int32BE (-1)             -- type modifier
        <> B.int16BE 0                -- format (text)

buildDataRow :: [ByteString] -> Builder
buildDataRow cols =
  buildBackendMsg 0x44 $
    B.int16BE (fromIntegral (length cols))
    <> foldMap (\v -> B.int32BE (fromIntegral (BS.length v)) <> B.byteString v) cols

buildErrorResponse :: ByteString -> ByteString -> Builder
buildErrorResponse sqlState msg =
  buildBackendMsg 0x45 $
    B.word8 (fromIntegral (fromEnum 'S')) <> B.byteString "ERROR" <> B.word8 0
    <> B.word8 (fromIntegral (fromEnum 'C')) <> B.byteString sqlState <> B.word8 0
    <> B.word8 (fromIntegral (fromEnum 'M')) <> B.byteString msg <> B.word8 0
    <> B.word8 0 -- terminator

------------------------------------------------------------------------
-- Socket helpers
------------------------------------------------------------------------

sendBuilder :: NS.Socket -> Builder -> IO ()
sendBuilder sock b = NSB.sendAll sock (LBS.toStrict (B.toLazyByteString b))

recvN :: NS.Socket -> Int -> IO ByteString
recvN sock n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
      chunk <- NSB.recv sock (min 65536 remaining)
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (remaining - BS.length chunk) (chunk : acc)

recvMaybe :: NS.Socket -> Int -> IO (Maybe ByteString)
recvMaybe sock n = do
  bs <- NSB.recv sock n
  if BS.null bs then pure Nothing else pure (Just bs)

decodeInt32BE :: ByteString -> Int -> Int32
decodeInt32BE bs off =
  let b0 = fromIntegral (BS.index bs off) :: Int32
      b1 = fromIntegral (BS.index bs (off + 1)) :: Int32
      b2 = fromIntegral (BS.index bs (off + 2)) :: Int32
      b3 = fromIntegral (BS.index bs (off + 3)) :: Int32
   in b0 `shiftL` 24 .|. b1 `shiftL` 16 .|. b2 `shiftL` 8 .|. b3

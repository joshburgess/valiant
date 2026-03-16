-- | Low-level PostgreSQL wire protocol I\/O.
--
-- 'WireConn' abstracts over a TCP or TLS connection, providing
-- send\/recv operations for protocol messages. This module handles
-- message framing (tag + length + payload), buffering, and
-- TLS upgrades via the SSLRequest subprotocol.
--
-- Most users should use 'PgWire.Connection' instead of this module.
module PgWire.Wire
  ( -- * Wire connection
    WireConn (..)
    -- * Connecting
  , connectTcp
  , connectTcpTimeout
    -- * TLS
  , upgradeTls
    -- * Sending messages
  , sendFrontendMsg
  , sendFrontendMsgs
  , sendRawBytes
    -- * Receiving messages
  , recvBackendMsg
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Time (NominalDiffTime)
import Data.Word (Word8)
import PgWire.Error (HsqlxError (..), throwHsqlx)
import System.Timeout (timeout)
import PgWire.Protocol.Backend (BackendMsg)
import PgWire.Protocol.Builders (buildFrontendMsg)
import PgWire.Protocol.Frontend (FrontendMsg)
import PgWire.Protocol.Parsers (parseBackendMsg)
import Network.Socket (Socket)
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB
import Network.Socket (setSocketOption, SocketOption(..))
import Network.TLS qualified as TLS
import Network.TLS (ClientParams (..), Supported (..), Shared (..))
import System.X509 (getSystemCertificateStore)

-- | Abstraction over a Postgres wire connection (plain TCP or TLS).
data WireConn = WireConn
  { wcSend :: ByteString -> IO ()
  , wcRecv :: Int -> IO ByteString
  , wcClose :: IO ()
  , wcBuffer :: IORef ByteString
  }

-- | Connect via TCP to the given host and port (no timeout).
connectTcp :: NS.HostName -> NS.PortNumber -> IO WireConn
connectTcp = connectTcpTimeout 0

-- | Connect via TCP with a timeout in seconds (0 = no timeout).
connectTcpTimeout :: NominalDiffTime -> NS.HostName -> NS.PortNumber -> IO WireConn
connectTcpTimeout timeoutSecs host port = do
  let hints = NS.defaultHints {NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
  case addrs of
    [] -> throwHsqlx (ConnectionError "No address found")
    (addr : _) -> do
      sock <- NS.socket (NS.addrFamily addr) NS.Stream NS.defaultProtocol
      let doConnect = NS.connect sock (NS.addrAddress addr)
      if timeoutSecs > 0
        then do
          let micros = round (timeoutSecs * 1000000) :: Int
          result <- timeout micros doConnect
          case result of
            Nothing -> do
              NS.close sock
              throwHsqlx (ConnectionError "Connect timed out")
            Just () -> pure ()
        else doConnect
      -- Disable Nagle's algorithm for lower latency on small messages
      setSocketOption sock NoDelay 1
      mkWireConn sock

mkWireConn :: Socket -> IO WireConn
mkWireConn sock = do
  buf <- newIORef BS.empty
  pure
    WireConn
      { wcSend = sendAll sock
      , wcRecv = recvExact sock buf
      , wcClose = NS.close sock
      , wcBuffer = buf
      }

-- | Upgrade a TCP WireConn to TLS. Sends the SSLRequest message,
-- checks the server response, and performs the TLS handshake.
-- Returns a new WireConn that sends/receives over TLS.
upgradeTls :: WireConn -> NS.HostName -> IO WireConn
upgradeTls wc hostname = do
  -- Send SSLRequest: [length=8 :: Int32] [code=80877103 :: Int32]
  -- This is a special message (no tag byte), like Startup.
  let sslRequest = LBS.toStrict . B.toLazyByteString $
        B.int32BE 8 <> B.int32BE 80877103
  wcSend wc sslRequest

  -- Read 1-byte response: 'S' = proceed with TLS, 'N' = refused
  resp <- wcRecv wc 1
  case BS.index resp 0 of
    83 {- S -} -> do
      -- Server accepted SSL. Perform TLS handshake.
      -- We need the raw socket back — but we abstracted it away.
      -- Use a TLS backend that wraps our send/recv functions.
      store <- getSystemCertificateStore
      let baseParams = TLS.defaultParamsClient hostname ""
          clientParams = baseParams
            { clientSupported = (clientSupported baseParams)
                { supportedVersions = [TLS.TLS13, TLS.TLS12]
                }
            , clientShared = (clientShared baseParams)
                { sharedCAStore = store
                }
            }
      -- Create a TLS context using our WireConn as the backend
      ctx <- TLS.contextNew (wireBackend wc) clientParams
      TLS.handshake ctx
      mkTlsWireConn ctx (wcBuffer wc)

    78 {- N -} ->
      throwHsqlx (ConnectionError "Server does not support SSL")

    other ->
      throwHsqlx (ConnectionError ("Unexpected SSL response: " <> BS8.pack (show other)))

-- | Wrap a WireConn as a TLS backend (for the handshake, which needs
-- raw socket I/O).
wireBackend :: WireConn -> TLS.Backend
wireBackend wc = TLS.Backend
  { TLS.backendFlush = pure ()
  , TLS.backendClose = wcClose wc
  , TLS.backendSend = wcSend wc
  , TLS.backendRecv = \n -> do
      -- TLS library expects exactly n bytes
      wcRecv wc n
  }

mkTlsWireConn :: TLS.Context -> IORef ByteString -> IO WireConn
mkTlsWireConn ctx bufRef = do
  -- Reset the buffer since TLS has its own framing
  writeIORef bufRef BS.empty
  tlsBuf <- newIORef BS.empty
  pure
    WireConn
      { wcSend = TLS.sendData ctx . LBS.fromStrict
      , wcRecv = tlsRecvExact ctx tlsBuf
      , wcClose = TLS.bye ctx >> TLS.contextClose ctx
      , wcBuffer = tlsBuf
      }

-- | Receive exactly n bytes from a TLS context, buffering leftovers.
tlsRecvExact :: TLS.Context -> IORef ByteString -> Int -> IO ByteString
tlsRecvExact ctx bufRef n = do
  buf <- readIORef bufRef
  go buf n []
  where
    go buf remaining acc
      | BS.length buf >= remaining = do
          let (taken, rest) = BS.splitAt remaining buf
          writeIORef bufRef rest
          pure (BS.concat (reverse (taken : acc)))
      | BS.null buf = do
          chunk <- TLS.recvData ctx
          if BS.null chunk
            then throwHsqlx (ConnectionError "TLS connection closed")
            else go chunk remaining acc
      | otherwise = do
          let remaining' = remaining - BS.length buf
          chunk <- TLS.recvData ctx
          if BS.null chunk
            then throwHsqlx (ConnectionError "TLS connection closed")
            else go chunk remaining' (buf : acc)

-- | Send a frontend message over the wire.
sendFrontendMsg :: WireConn -> FrontendMsg -> IO ()
sendFrontendMsg wc msg = wcSend wc (buildFrontendMsg msg)
{-# INLINE sendFrontendMsg #-}

-- | Send multiple frontend messages in a single syscall (message coalescing).
-- This is critical for pipelining: Bind+Execute+Bind+Execute+...+Sync
-- should be sent as one TCP segment, not N separate sends.
sendFrontendMsgs :: WireConn -> [FrontendMsg] -> IO ()
sendFrontendMsgs wc msgs = wcSend wc (BS.concat (map buildFrontendMsg msgs))
{-# INLINE sendFrontendMsgs #-}

-- | Send raw bytes (for startup message which has a different format).
sendRawBytes :: WireConn -> ByteString -> IO ()
sendRawBytes wc = wcSend wc

-- | Receive and parse a single backend message.
recvBackendMsg :: WireConn -> IO BackendMsg
recvBackendMsg wc = do
  -- Read tag (1 byte) + length (4 bytes) together in a single recv
  header <- wcRecv wc 5
  let !tag = BS.index header 0
      !len = decodeInt32At header 1
      !payloadLen = len - 4
  -- Read payload
  payload <-
    if payloadLen > 0
      then wcRecv wc (fromIntegral payloadLen)
      else pure BS.empty
  case parseBackendMsg tag payload of
    Left err -> throwHsqlx (ProtocolError (BS8.pack err))
    Right msg -> pure msg
  where
    decodeInt32At :: ByteString -> Int -> Int
    decodeInt32At bs off =
      let !b0 = fromIntegral (BS.index bs off)
          !b1 = fromIntegral (BS.index bs (off + 1))
          !b2 = fromIntegral (BS.index bs (off + 2))
          !b3 = fromIntegral (BS.index bs (off + 3))
       in b0 * 16777216 + b1 * 65536 + b2 * 256 + b3

-- Socket helpers ----------------------------------------------------------

sendAll :: Socket -> ByteString -> IO ()
sendAll sock bs
  | BS.null bs = pure ()
  | otherwise = do
      sent <- NSB.send sock bs
      sendAll sock (BS.drop sent bs)

-- | Receive exactly @n@ bytes, using the buffer for leftovers.
recvExact :: Socket -> IORef ByteString -> Int -> IO ByteString
recvExact sock bufRef n = do
  buf <- readIORef bufRef
  go buf n []
  where
    go buf remaining acc
      | BS.length buf >= remaining = do
          let (taken, rest) = BS.splitAt remaining buf
          writeIORef bufRef rest
          pure (BS.concat (reverse (taken : acc)))
      | BS.null buf = do
          chunk <- NSB.recv sock 8192
          if BS.null chunk
            then throwHsqlx (ConnectionError "Connection closed by server")
            else go chunk remaining acc
      | otherwise = do
          -- Use what we have in the buffer, then read more
          let remaining' = remaining - BS.length buf
          chunk <- NSB.recv sock 8192
          if BS.null chunk
            then throwHsqlx (ConnectionError "Connection closed by server")
            else go chunk remaining' (buf : acc)

module Hsqlx.Wire
  ( WireConn (..)
  , connectTcp
  , sendFrontendMsg
  , sendRawBytes
  , recvBackendMsg
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Word (Word8)
import Hsqlx.Error (HsqlxError (..), throwHsqlx)
import Hsqlx.Protocol.Backend (BackendMsg)
import Hsqlx.Protocol.Builders (buildFrontendMsg)
import Hsqlx.Protocol.Frontend (FrontendMsg)
import Hsqlx.Protocol.Parsers (parseBackendMsg)
import Network.Socket (Socket)
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB

-- | Abstraction over a Postgres wire connection (plain TCP or TLS).
data WireConn = WireConn
  { wcSend :: ByteString -> IO ()
  , wcRecv :: Int -> IO ByteString
  , wcClose :: IO ()
  , wcBuffer :: IORef ByteString
  }

-- | Connect via TCP to the given host and port.
connectTcp :: NS.HostName -> NS.PortNumber -> IO WireConn
connectTcp host port = do
  let hints = NS.defaultHints {NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
  case addrs of
    [] -> throwHsqlx (ConnectionError "No address found")
    (addr : _) -> do
      sock <- NS.socket (NS.addrFamily addr) NS.Stream NS.defaultProtocol
      NS.connect sock (NS.addrAddress addr)
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

-- | Send a frontend message over the wire.
sendFrontendMsg :: WireConn -> FrontendMsg -> IO ()
sendFrontendMsg wc msg = wcSend wc (buildFrontendMsg msg)

-- | Send raw bytes (for startup message which has a different format).
sendRawBytes :: WireConn -> ByteString -> IO ()
sendRawBytes wc = wcSend wc

-- | Receive and parse a single backend message.
recvBackendMsg :: WireConn -> IO BackendMsg
recvBackendMsg wc = do
  -- Read 1-byte tag
  tagBs <- wcRecv wc 1
  let tag = BS.index tagBs 0 :: Word8
  -- Read 4-byte length
  lenBs <- wcRecv wc 4
  let len = decodeInt32BE lenBs
      payloadLen = len - 4
  -- Read payload
  payload <-
    if payloadLen > 0
      then wcRecv wc (fromIntegral payloadLen)
      else pure BS.empty
  case parseBackendMsg tag payload of
    Left err -> throwHsqlx (ProtocolError (BS8.pack err))
    Right msg -> pure msg
  where
    -- Inline helper to avoid depending on binary codecs for protocol parsing
    decodeInt32BE :: ByteString -> Int
    decodeInt32BE bs =
      let b0 = fromIntegral (BS.index bs 0)
          b1 = fromIntegral (BS.index bs 1)
          b2 = fromIntegral (BS.index bs 2)
          b3 = fromIntegral (BS.index bs 3)
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
            else go chunk (remaining - 0) acc -- didn't consume buf yet since it was empty
      | otherwise = do
          -- Use what we have in the buffer, then read more
          let remaining' = remaining - BS.length buf
          chunk <- NSB.recv sock 8192
          if BS.null chunk
            then throwHsqlx (ConnectionError "Connection closed by server")
            else go chunk remaining' (buf : acc)

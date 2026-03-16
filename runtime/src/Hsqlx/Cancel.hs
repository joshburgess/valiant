-- | Query cancellation via the CancelRequest protocol.
module Hsqlx.Cancel
  ( cancelQuery
  , withQueryTimeout
  ) where

import Control.Concurrent.Async (race)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32)
import Data.Time (NominalDiffTime)
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB
import Hsqlx.Connection (Connection (..))
import Hsqlx.Connection.Config (ConnConfig (..))
import Hsqlx.Error (HsqlxError (..), throwHsqlx)

-- | Cancel the currently executing query on a connection.
cancelQuery :: Connection -> IO ()
cancelQuery conn = do
  let cfg = connConfig conn
      pid = connBackendPid conn
      key = connBackendKey conn
  _ <- try @SomeException $ sendCancelRequest (ccHost cfg) (ccPort cfg) pid key
  pure ()

-- | Run an IO action with a timeout. If the timeout expires, the query
-- is cancelled and an error is thrown.
withQueryTimeout :: Connection -> NominalDiffTime -> IO a -> IO a
withQueryTimeout conn seconds action = do
  let micros = round (seconds * 1000000) :: Int
  result <- race (threadDelay micros) action
  case result of
    Left () -> do
      cancelQuery conn
      throwHsqlx (ConnectionError "Query timed out")
    Right a -> pure a

sendCancelRequest :: NS.HostName -> NS.PortNumber -> Int32 -> Int32 -> IO ()
sendCancelRequest host port pid key = do
  let hints = NS.defaultHints {NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
  case addrs of
    [] -> pure ()
    (addr : _) -> do
      sock <- NS.socket (NS.addrFamily addr) NS.Stream NS.defaultProtocol
      NS.connect sock (NS.addrAddress addr)
      let msg = LBS.toStrict . B.toLazyByteString $
            B.int32BE 16
              <> B.int32BE 80877102
              <> B.int32BE pid
              <> B.int32BE key
      NSB.sendAll sock msg
      NS.close sock

-- | Query cancellation via the CancelRequest protocol.
--
-- PostgreSQL's cancel mechanism works by opening a separate TCP connection
-- and sending a CancelRequest message containing the backend PID and
-- secret key from the original connection's BackendKeyData. The server
-- then signals the backend process to cancel its current query.
--
-- @
-- -- Cancel a long-running query from another thread:
-- 'cancelQuery' conn
--
-- -- Or use a timeout wrapper:
-- 'withQueryTimeout' conn 5.0 $ do
--   fetchAll conn expensiveQuery ()
-- @
module PgWire.Cancel
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
import PgWire.Connection (Connection (..))
import PgWire.Connection.Config (ConnConfig (..))
import PgWire.Error (PgWireError (..), throwPgWire)

-- | Cancel the currently executing query on a connection.
--
-- Opens a separate TCP connection to the same Postgres server and sends
-- a CancelRequest message. The original query will receive an ErrorResponse
-- with SQLSTATE 57014 (query_canceled).
--
-- This is safe to call from any thread.
cancelQuery :: Connection -> IO ()
cancelQuery conn = do
  let cfg = connConfig conn
      pid = connBackendPid conn
      key = connBackendKey conn
  result <- try @SomeException $ sendCancelRequest (ccHost cfg) (ccPort cfg) pid key
  case result of
    Right () -> pure ()
    Left _ -> pure () -- Best-effort: cancel may fail if server is unreachable

-- | Run an IO action with a timeout. If the timeout expires, the query
-- is cancelled and a 'PoolTimeout' error is thrown.
--
-- > withQueryTimeout conn 5.0 $ do
-- >   fetchAll conn someExpensiveQuery ()
withQueryTimeout :: Connection -> NominalDiffTime -> IO a -> IO a
withQueryTimeout conn seconds action = do
  let micros = round (seconds * 1000000) :: Int
  result <- race (threadDelay micros) action
  case result of
    Left () -> do
      cancelQuery conn
      throwPgWire (ConnectionError "Query timed out")
    Right a -> pure a

-- | Send a CancelRequest to the given host/port.
--
-- CancelRequest format (not a normal frontend message):
--   [length :: Int32 = 16]
--   [cancel code :: Int32 = 80877102]
--   [pid :: Int32]
--   [key :: Int32]
sendCancelRequest :: NS.HostName -> NS.PortNumber -> Int32 -> Int32 -> IO ()
sendCancelRequest host port pid key = do
  let hints = NS.defaultHints {NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
  case addrs of
    [] -> throwPgWire (ConnectionError "Cancel: no address found")
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

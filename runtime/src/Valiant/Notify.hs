-- | LISTEN/NOTIFY support for PostgreSQL asynchronous notifications.
--
-- == Delivery model
--
-- valiant's reader thread parks between queries, so NOTIFY messages that
-- arrive while the connection is idle are not dispatched until the next
-- query wakes the reader. 'waitForNotification' and its timeout variant
-- issue an empty simple query to flush pending NOTIFYs, then block on a
-- one-shot MVar filled by the handler.
--
-- In practice this means:
--
-- * Call 'waitForNotificationTimeout' (not 'waitForNotification') in
--   production code. A hung delivery returns 'Nothing' after the
--   deadline rather than blocking the caller forever.
--
-- * For fire-and-forget \"wake me whenever a NOTIFY arrives\" semantics,
--   poll: call 'waitForNotificationTimeout' in a loop with a short
--   timeout. Every iteration re-flushes the socket.
module Valiant.Notify
  ( Notification (..)
  , listen
  , unlisten
  , waitForNotification
  , waitForNotificationTimeout
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Int (Int32)
import PgWire.Async (AsyncWireConn (..))
import PgWire.Connection (Connection (..), simpleQuery)

-- | A notification received from PostgreSQL via @NOTIFY@ or @pg_notify()@.
data Notification = Notification
  { notifPid :: Int32
  -- ^ Process ID of the notifying backend.
  , notifChannel :: ByteString
  -- ^ Channel name the notification was sent on.
  , notifPayload :: ByteString
  -- ^ Optional payload string (empty if none was sent).
  }
  deriving stock (Show, Eq)

-- | Subscribe to a PostgreSQL notification channel by issuing @LISTEN@.
-- The channel name is automatically quoted as an identifier.
-- Use 'waitForNotification' or 'waitForNotificationTimeout' to receive
-- notifications after subscribing.
listen :: Connection -> ByteString -> IO ()
listen conn channel = do
  _ <- simpleQuery conn ("LISTEN " <> quoteIdent channel)
  pure ()

-- | Unsubscribe from a PostgreSQL notification channel by issuing @UNLISTEN@.
-- The channel name is automatically quoted as an identifier.
unlisten :: Connection -> ByteString -> IO ()
unlisten conn channel = do
  _ <- simpleQuery conn ("UNLISTEN " <> quoteIdent channel)
  pure ()

-- | Block until a notification arrives on any subscribed channel.
--
-- Registers a one-shot handler, sends an empty simple query to flush
-- any NOTIFYs already queued on the socket, then blocks on the handler.
-- If no NOTIFY was already pending at the time of the flush, this call
-- blocks until the next query on this connection wakes the reader.
-- Prefer 'waitForNotificationTimeout' for production use.
waitForNotification :: Connection -> IO Notification
waitForNotification conn = do
  notifVar <- newEmptyMVar
  installNotifyHandler conn notifVar
  -- Send an empty query to flush any pending notifications from the server
  _ <- simpleQuery conn ""
  takeMVar notifVar

-- | Wait for a notification with a timeout (in seconds).
-- Returns 'Nothing' if the timeout expires.
waitForNotificationTimeout :: Connection -> Double -> IO (Maybe Notification)
waitForNotificationTimeout conn seconds = do
  notifVar <- newEmptyMVar
  installNotifyHandler conn notifVar
  _ <- simpleQuery conn ""
  let micros = round (seconds * 1000000) :: Int
  result <- race (threadDelay micros) (takeMVar notifVar)
  -- Restore default handler regardless of outcome
  writeIORef (awcNotifyHandler (connAsync conn)) (\_ _ _ -> pure ())
  pure $ case result of
    Left () -> Nothing
    Right n -> Just n

-- Internal ------------------------------------------------------------------

-- | Install a one-shot notification handler that fills the given MVar.
installNotifyHandler :: Connection -> MVar Notification -> IO ()
installNotifyHandler conn notifVar = do
  let handler pid channel payload = do
        let notif = Notification pid channel payload
        _ <- tryPutMVar notifVar notif
        -- Restore the default no-op handler after delivery
        writeIORef (awcNotifyHandler (connAsync conn)) (\_ _ _ -> pure ())
  writeIORef (awcNotifyHandler (connAsync conn)) handler

-- | Simple identifier quoting (double-quote).
quoteIdent :: ByteString -> ByteString
quoteIdent ident = "\"" <> BS8.concatMap escapeQuote ident <> "\""
  where
    escapeQuote '"' = "\"\""
    escapeQuote c = BS8.singleton c

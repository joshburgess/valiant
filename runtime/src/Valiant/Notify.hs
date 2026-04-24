-- | LISTEN/NOTIFY support for PostgreSQL asynchronous notifications.
--
-- Notifications are dispatched by the reader thread as they arrive,
-- calling the registered handler inline. 'waitForNotification' blocks
-- the caller by registering a one-shot callback.
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
-- Registers a one-shot handler on the connection's notification callback.
-- The reader thread will fill the MVar when a NotificationResponse arrives.
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

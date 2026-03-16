-- | LISTEN/NOTIFY support for PostgreSQL asynchronous notifications.
module Hsqlx.Notify
  ( Notification (..)
  , listen
  , unlisten
  , waitForNotification
  , waitForNotificationTimeout
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32)
import PgWire.Connection (Connection (..), simpleQuery)
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Protocol.Backend
import PgWire.Wire (recvBackendMsg)

-- | A notification received from PostgreSQL.
data Notification = Notification
  { notifPid :: Int32
  , notifChannel :: ByteString
  , notifPayload :: ByteString
  }
  deriving stock (Show, Eq)

-- | Subscribe to a channel.
listen :: Connection -> ByteString -> IO ()
listen conn channel = do
  _ <- simpleQuery conn ("LISTEN " <> quoteIdent channel)
  pure ()

-- | Unsubscribe from a channel.
unlisten :: Connection -> ByteString -> IO ()
unlisten conn channel = do
  _ <- simpleQuery conn ("UNLISTEN " <> quoteIdent channel)
  pure ()

-- | Block until a notification arrives on any subscribed channel.
waitForNotification :: Connection -> IO Notification
waitForNotification conn = do
  -- Send an empty query to flush any pending notifications
  _ <- simpleQuery conn ""
  pollNotification conn

-- | Wait for a notification with a timeout (in seconds).
-- Returns 'Nothing' if the timeout expires.
waitForNotificationTimeout :: Connection -> Double -> IO (Maybe Notification)
waitForNotificationTimeout conn seconds = do
  _ <- simpleQuery conn ""
  let micros = round (seconds * 1000000) :: Int
  result <- race (threadDelay micros) (pollNotification conn)
  pure $ case result of
    Left () -> Nothing
    Right n -> Just n

-- | Poll the connection for notification messages, skipping other async
-- messages (ParameterStatus, NoticeResponse).
pollNotification :: Connection -> IO Notification
pollNotification conn = do
  msg <- recvBackendMsg (connWire conn)
  case msg of
    NotificationResponse pid channel payload ->
      pure (Notification pid channel payload)
    ParameterStatus _ _ -> pollNotification conn
    NoticeResponse _ -> pollNotification conn
    other ->
      throwHsqlx (ProtocolError ("Unexpected while waiting for notification: " <> BS8.pack (show other)))

-- | Simple identifier quoting (double-quote).
quoteIdent :: ByteString -> ByteString
quoteIdent ident = "\"" <> BS8.concatMap escapeQuote ident <> "\""
  where
    escapeQuote '"' = "\"\""
    escapeQuote c = BS8.singleton c

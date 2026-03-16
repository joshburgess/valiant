-- | Logging hooks for query timing and connection events.
module Hsqlx.Logging
  ( LogEvent (..)
  , LogLevel (..)
  , Logger
  , nullLogger
  , stderrLogger
  , withQueryLogging
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import System.IO (hPutStrLn, stderr)

-- | Log severity levels.
data LogLevel = Debug | Info | Warn | Error
  deriving stock (Show, Eq, Ord)

-- | Events that can be logged.
data LogEvent
  = QueryStart ByteString
  | QueryComplete ByteString NominalDiffTime Int
  -- ^ SQL, duration, rows returned
  | QueryError ByteString NominalDiffTime ByteString
  -- ^ SQL, duration, error message
  | ConnectionOpened ByteString
  -- ^ Connection string (masked)
  | ConnectionClosed
  | PoolAcquire NominalDiffTime
  -- ^ Time waited for a connection
  | PoolRelease
  | PoolTimeout
  deriving stock (Show)

-- | A logging callback.
type Logger = LogLevel -> LogEvent -> IO ()

-- | Logger that discards all events.
nullLogger :: Logger
nullLogger _ _ = pure ()

-- | Simple logger that writes to stderr.
stderrLogger :: LogLevel -> Logger
stderrLogger minLevel level event
  | level >= minLevel = hPutStrLn stderr (formatEvent level event)
  | otherwise = pure ()

-- | Bracket a query with timing and logging.
withQueryLogging :: Logger -> ByteString -> IO a -> IO a
withQueryLogging logger sql action = do
  logger Debug (QueryStart sql)
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  let elapsed = diffUTCTime end start
  logger Debug (QueryComplete sql elapsed 0)
  pure result

formatEvent :: LogLevel -> LogEvent -> String
formatEvent level event =
  "[hsqlx:" <> show level <> "] " <> case event of
    QueryStart sql -> "query start: " <> trunc 80 (BS8.unpack sql)
    QueryComplete sql dur rows ->
      "query complete: " <> trunc 60 (BS8.unpack sql)
        <> " (" <> show dur <> ", " <> show rows <> " rows)"
    QueryError sql dur msg ->
      "query error: " <> trunc 60 (BS8.unpack sql)
        <> " (" <> show dur <> "): " <> BS8.unpack msg
    ConnectionOpened cs -> "connection opened: " <> BS8.unpack cs
    ConnectionClosed -> "connection closed"
    PoolAcquire dur -> "pool acquire (" <> show dur <> ")"
    PoolRelease -> "pool release"
    PoolTimeout -> "pool timeout"
  where
    trunc n s
      | length s <= n = s
      | otherwise = take n s <> "..."

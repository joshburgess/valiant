-- | Logging hooks for query timing and connection events.
module Valiant.Logging
  ( LogEvent (..)
  , LogLevel (..)
  , Logger
  , nullLogger
  , stderrLogger
  , withQueryLogging
  , poolLoggerFromLogger
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import PgWire.Pool.Config (PoolLogger)
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
  | PoolCreatedConn
  -- ^ A new connection was created in the pool
  | PoolDestroyedConn ByteString
  -- ^ A connection was destroyed. Includes reason.
  | PoolRecycled
  -- ^ A connection was recycled (health check passed)
  | PoolReaperSwept Int
  -- ^ Reaper swept N idle/expired connections
  | PoolWarmerCreated Int
  -- ^ Warmer created N connections to maintain min-idle
  | PoolResized Int Int
  -- ^ Pool resized from old size to new size
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

-- | Create a 'PoolLogger' that forwards pool events to a 'Logger' as
-- structured 'LogEvent's. Pass this as the @poolLogger@ field when
-- constructing a 'PgWire.Pool.Config.PoolConfig'.
--
-- @
-- let cfg = defaultPoolConfig
--       { poolLogger = poolLoggerFromLogger (stderrLogger Info)
--       }
-- @
poolLoggerFromLogger :: Logger -> PoolLogger
poolLoggerFromLogger logger level msg = logger valiantLevel event
  where
    valiantLevel = case level of
      "debug" -> Debug
      "info" -> Info
      "warn" -> Warn
      _ -> Info
    event = parsePoolEvent msg

-- | Parse the pool's log message into a structured 'LogEvent'.
-- Falls back to a generic 'PoolDestroyedConn' with the raw message
-- if the pattern doesn't match a known event.
parsePoolEvent :: ByteString -> LogEvent
parsePoolEvent msg
  | msg == "created connection" = PoolCreatedConn
  | msg == "recycled connection" = PoolRecycled
  | msg == "acquire timeout" = PoolTimeout
  | "destroyed connection: " `BS8.isPrefixOf` msg =
      PoolDestroyedConn (BS8.drop 22 msg)
  | "reaper swept " `BS8.isPrefixOf` msg =
      case BS8.readInt (BS8.drop 14 msg) of
        Just (n, _) -> PoolReaperSwept n
        Nothing -> PoolDestroyedConn msg
  | "warmer created " `BS8.isPrefixOf` msg =
      case BS8.readInt (BS8.drop 15 msg) of
        Just (n, _) -> PoolWarmerCreated n
        Nothing -> PoolDestroyedConn msg
  | "resized from " `BS8.isPrefixOf` msg =
      case parseResized (BS8.drop 13 msg) of
        Just (old, new') -> PoolResized old new'
        Nothing -> PoolDestroyedConn msg
  | otherwise = PoolDestroyedConn msg
  where
    parseResized bs = do
      (old, rest) <- BS8.readInt bs
      -- rest should be " to N"
      let rest' = BS8.drop 4 rest -- drop " to "
      (new', _) <- BS8.readInt rest'
      pure (old, new')

formatEvent :: LogLevel -> LogEvent -> String
formatEvent level event =
  "[valiant:" <> show level <> "] " <> case event of
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
    PoolCreatedConn -> "pool created connection"
    PoolDestroyedConn reason -> "pool destroyed connection: " <> BS8.unpack reason
    PoolRecycled -> "pool recycled connection"
    PoolReaperSwept n -> "pool reaper swept " <> show n <> " connections"
    PoolWarmerCreated n -> "pool warmer created " <> show n <> " connections"
    PoolResized old new' -> "pool resized from " <> show old <> " to " <> show new'
  where
    trunc n s
      | length s <= n = s
      | otherwise = take n s <> "..."

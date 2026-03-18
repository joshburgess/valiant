-- | Configuration types for the connection pool.
--
-- Use 'defaultPoolConfig' as a starting point and override fields:
--
-- @
-- cfg = 'defaultPoolConfig'
--   { poolConnString = \"postgres:\/\/user:pass\@localhost\/mydb\"
--   , poolSize = 20
--   , poolRecyclingMethod = 'RecycleVerified'
--   }
-- @
module PgWire.Pool.Config
  ( PoolConfig (..)
  , defaultPoolConfig
  , RecyclingMethod (..)
  , QueueMode (..)
  , PoolLogger
  , nullPoolLogger
  ) where

import Data.ByteString (ByteString)
import Data.Time (NominalDiffTime)
import PgWire.Pool.Observation (PoolObserver, nullObserver)

-- | How to validate a connection before handing it to the caller.
data RecyclingMethod
  = RecycleFast
  -- ^ Check @awcAlive@ TVar only — free, no I/O.
  | RecycleVerified
  -- ^ Send an empty query if the connection has been idle longer than
  -- 'poolHealthCheckAge'. Falls back to 'RecycleFast' for recently used
  -- connections.
  | RecycleClean
  -- ^ Send @DISCARD ALL@ before every reuse (resets all session state).
  deriving stock (Show, Eq)

-- | Which end of the idle queue to draw from.
data QueueMode
  = QueueFIFO
  -- ^ Oldest idle connection first — spreads load across backends.
  | QueueLIFO
  -- ^ Newest idle connection first — keeps fewer total connections alive.
  deriving stock (Show, Eq)

-- | A pool logging callback. The pool calls this with a severity string
-- (@\"debug\"@, @\"info\"@, @\"warn\"@) and a message.
type PoolLogger = ByteString -> ByteString -> IO ()

-- | Logger that discards all events.
nullPoolLogger :: PoolLogger
nullPoolLogger _ _ = pure ()

-- | Configuration for a connection pool. Use 'defaultPoolConfig' and
-- override the fields you need:
--
-- @
-- pool <- 'PgWire.Pool.newPool' defaultPoolConfig
--   { poolConnString = \"postgres:\/\/user:pass\@localhost:5432\/mydb\"
--   , poolSize = 20
--   }
-- @
data PoolConfig = PoolConfig
  { poolConnString :: ByteString
  -- ^ PostgreSQL connection string. Required.
  , poolSize :: !Int
  -- ^ Maximum number of connections. Default: 10.
  , poolIdleTime :: !NominalDiffTime
  -- ^ Close idle connections after this duration. Default: 600s (10 min).
  , poolMaxLife :: !NominalDiffTime
  -- ^ Close connections after this total lifetime. Default: 3600s (1 hour).
  , poolAcquireTimeout :: !NominalDiffTime
  -- ^ Maximum time to wait for a connection. Throws 'PgWire.Error.PoolTimeout'
  -- if exceeded. Default: 10s.
  , poolMinIdle :: !Int
  -- ^ Minimum idle connections to maintain. A background warmer thread
  -- creates connections to fill the deficit. Default: 0 (no warming).
  , poolReaperInterval :: !NominalDiffTime
  -- ^ How often the reaper sweeps idle/expired connections. Default: 30s.
  , poolRecyclingMethod :: !RecyclingMethod
  -- ^ How to validate connections before reuse. Default: 'RecycleFast'.
  , poolQueueMode :: !QueueMode
  -- ^ Which end of the idle queue to draw from. Default: 'QueueLIFO'.
  , poolHealthCheckAge :: !NominalDiffTime
  -- ^ Only verify (empty query) if a connection has been idle longer than
  -- this. Only relevant when 'poolRecyclingMethod' is 'RecycleVerified'.
  -- Default: 5s.
  , poolMaxLifeJitter :: !NominalDiffTime
  -- ^ Randomize 'poolMaxLife' ± this amount to avoid thundering herd
  -- expiration. Default: 60s.
  , poolConnectionRetries :: !Int
  -- ^ Number of retries when creating a connection fails. Retries use
  -- exponential backoff (100ms, 200ms, 400ms, ...). Default: 3.
  , poolLogger :: PoolLogger
  -- ^ Logging callback. Default: 'nullPoolLogger' (discards all events).
  , poolObserver :: PoolObserver
  -- ^ Structured event callback for metrics and monitoring.
  -- Called for every significant pool state transition.
  -- Default: 'nullObserver' (discards all events).
  -- See "PgWire.Pool.Observation" for the event types.
  }

-- Manual Show instance that omits the function field.
instance Show PoolConfig where
  show cfg = unwords
    [ "PoolConfig"
    , "{poolConnString=" <> show (poolConnString cfg)
    , ",poolSize=" <> show (poolSize cfg)
    , ",poolIdleTime=" <> show (poolIdleTime cfg)
    , ",poolMaxLife=" <> show (poolMaxLife cfg)
    , ",poolAcquireTimeout=" <> show (poolAcquireTimeout cfg)
    , ",poolMinIdle=" <> show (poolMinIdle cfg)
    , ",poolReaperInterval=" <> show (poolReaperInterval cfg)
    , ",poolRecyclingMethod=" <> show (poolRecyclingMethod cfg)
    , ",poolQueueMode=" <> show (poolQueueMode cfg)
    , ",poolHealthCheckAge=" <> show (poolHealthCheckAge cfg)
    , ",poolMaxLifeJitter=" <> show (poolMaxLifeJitter cfg)
    , ",poolConnectionRetries=" <> show (poolConnectionRetries cfg)
    , ",poolLogger=<function>"
    , ",poolObserver=<function>}"
    ]

-- | Sensible defaults: 10 connections, 10-minute idle timeout, 1-hour max life,
-- 10-second acquire timeout, 'RecycleFast', 'QueueLIFO', no warming, no logging.
defaultPoolConfig :: PoolConfig
defaultPoolConfig =
  PoolConfig
    { poolConnString = ""
    , poolSize = 10
    , poolIdleTime = 600
    , poolMaxLife = 3600
    , poolAcquireTimeout = 10
    , poolMinIdle = 0
    , poolReaperInterval = 30
    , poolRecyclingMethod = RecycleFast
    , poolQueueMode = QueueLIFO
    , poolHealthCheckAge = 5
    , poolMaxLifeJitter = 60
    , poolConnectionRetries = 3
    , poolLogger = nullPoolLogger
    , poolObserver = nullObserver
    }

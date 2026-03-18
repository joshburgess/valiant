-- | Structured pool lifecycle events for observability.
--
-- Set an observation handler via 'PgWire.Pool.Config.poolObserver' to
-- receive events for every significant pool state transition. Use these
-- for metrics, structured logging, alerting, or debugging.
--
-- @
-- pool <- newPool defaultPoolConfig
--   { poolObserver = \\event -> do
--       case event of
--         ConnectionCreated -> incrementCounter \"pool.created\"
--         ConnectionDestroyed reason -> logInfo $ \"destroyed: \" <> reason
--         AcquireTimeout -> incrementCounter \"pool.timeouts\"
--         _ -> pure ()
--   }
-- @
module PgWire.Pool.Observation
  ( PoolEvent (..)
  , PoolObserver
  , nullObserver
  ) where

import Data.ByteString (ByteString)
import Data.Time (NominalDiffTime)

-- | A structured event emitted by the connection pool.
data PoolEvent
  = ConnectionCreated
  -- ^ A new connection was established.
  | ConnectionDestroyed !ByteString
  -- ^ A connection was closed. The 'ByteString' is the reason
  -- (e.g., @\"expired\"@, @\"unhealthy\"@, @\"reaped\"@, @\"pool closed\"@,
  -- @\"resize\"@, @\"exception\"@, @\"retain\"@).
  | ConnectionAcquired !NominalDiffTime
  -- ^ A connection was handed to a caller. The 'NominalDiffTime' is
  -- how long the caller waited (0 if a connection was immediately available).
  | ConnectionReleased
  -- ^ A connection was returned to the pool.
  | ConnectionRecycled
  -- ^ An idle connection passed its recycling check and was reused.
  | HealthCheckFailed
  -- ^ A connection failed its health check and was destroyed.
  | AcquireTimeout
  -- ^ A caller timed out waiting for a connection.
  | ReaperSwept !Int
  -- ^ The background reaper closed N idle/expired connections.
  | WarmerCreated !Int
  -- ^ The background warmer created N connections to fill min-idle.
  | PoolResized !Int !Int
  -- ^ Pool was resized from old size to new size.
  | PoolShutdown
  -- ^ 'closePool' was called.
  deriving stock (Show, Eq)

-- | Callback for pool events. Set in 'PgWire.Pool.Config.PoolConfig'.
type PoolObserver = PoolEvent -> IO ()

-- | Observer that discards all events.
nullObserver :: PoolObserver
nullObserver _ = pure ()

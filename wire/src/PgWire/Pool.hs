-- | Thread-safe connection pool for PostgreSQL.
--
-- Manages a set of reusable 'Connection's with configurable pool size,
-- idle timeout, max lifetime, health checking, and lifecycle hooks.
-- Inspired by deadpool-postgres (Rust).
--
-- @
-- pool <- 'newPool' 'PgWire.Pool.Config.defaultPoolConfig'
--   { poolConnString = \"postgres:\/\/...\"
--   , poolSize = 10
--   }
-- 'withResource' pool $ \\conn -> ...
-- 'closePool' pool
-- @
module PgWire.Pool
  ( Pool (..)
  , PoolStats (..)
  , newPool
  , closePool
  , drainPool
  , withResource
  , poolStats
  , poolIsAlive
  , resize
  , retain
  , setPostCreateHook
  , setOnAcquireHook
  , setPreReleaseHook
  , withResourceTimeout
  ) where

import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race)
import Control.Concurrent.STM
import Control.Exception (AsyncException, SomeException, catch, mask, onException, throwIO, try)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.HashPSQ qualified as PSQ
import PgWire.Async (AsyncWireConn (..))
import PgWire.Connection (Connection (..), close, connectString, simpleQuery)
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Pool.Config (PoolConfig (..), QueueMode (..), RecyclingMethod (..))
import PgWire.Pool.Observation (PoolEvent (ConnectionAcquired, ConnectionCreated, ConnectionDestroyed, ConnectionRecycled, ConnectionReleased, HealthCheckFailed, AcquireTimeout, ReaperSwept, WarmerCreated, PoolResized, PoolShutdown))
import PgWire.TypeCache (TypeCache, newTypeCache)
import System.Random (randomRIO)

-- | A thread-safe connection pool for PostgreSQL.
--
-- Manages a bounded set of reusable 'Connection's with idle timeout, max lifetime
-- (with jitter to avoid thundering-herd reconnects), health checking, and lifecycle hooks.
-- The pool is safe to share across threads. Use 'newPool' to create and 'closePool' to shut down.
data Pool = Pool
  { pConfig :: PoolConfig
  , pIdle :: TVar (Seq PoolEntry)
  , pActive :: TVar Int
  , pWaiters :: TVar (Seq (TMVar (Either HsqlxError Connection)))
  , pClosed :: TVar Bool
  , pReaper :: IORef (Async ())
  , pWarmer :: IORef (Maybe (Async ()))
  , pEffectiveSize :: TVar Int
  -- ^ Mutable pool size for runtime 'resize'.
  , pTotalCreated :: TVar Int
  , pTotalDestroyed :: TVar Int
  , pTotalTimeouts :: TVar Int
  , pConnMeta :: TVar (Map Int32 ConnMeta)
  -- ^ Per-connection metadata keyed by backend PID.
  -- Tracks creation time and jittered max-life for each connection.
  , pOnCreate :: IORef (Connection -> IO ())
  , pOnAcquire :: IORef (Connection -> IO ())
  , pOnRelease :: IORef (Connection -> IO ())
  , pTypeCache :: TypeCache
  -- ^ Pool-level cache for resolved PG type metadata (OID → TypeInfo).
  -- Avoids redundant @pg_type@ round-trips across connections.
  }

-- | Per-connection metadata, stored by backend PID.
data ConnMeta = ConnMeta
  { cmCreatedAt :: !UTCTime
  , cmDeadline :: !UTCTime
  -- ^ Creation time + jittered max-life. Connection is expired after this.
  }
  deriving stock (Generic)

instance NoThunks ConnMeta

data PoolEntry = PoolEntry
  { peConn :: Connection
  , peCreatedAt :: UTCTime
  , peLastUsed :: IORef UTCTime
  }

-- | Snapshot of pool statistics at a point in time, obtained via 'poolStats'.
data PoolStats = PoolStats
  { psIdle :: !Int
  -- ^ Number of connections currently idle in the pool.
  , psInUse :: !Int
  -- ^ Number of connections currently checked out by callers.
  , psWaiters :: !Int
  -- ^ Number of threads blocked waiting to acquire a connection.
  , psMaxSize :: !Int
  -- ^ Current maximum pool size (may differ from initial config after 'resize').
  , psTotalCreated :: !Int
  -- ^ Cumulative count of connections created since pool creation.
  , psTotalDestroyed :: !Int
  -- ^ Cumulative count of connections destroyed since pool creation.
  , psTotalTimeouts :: !Int
  -- ^ Cumulative count of acquire attempts that timed out.
  }
  deriving stock (Show, Eq, Generic)

instance NoThunks PoolStats

-- | Create a new connection pool from the given configuration.
--
-- Starts background threads for reaping idle\/expired connections and (if
-- @poolMinIdle > 0@) warming the pool to maintain a minimum number of idle
-- connections. No connections are created eagerly; the first 'withResource'
-- call triggers the initial connection.
--
-- Call 'closePool' when the pool is no longer needed to release all resources.
newPool :: PoolConfig -> IO Pool
newPool cfg = do
  idle <- newTVarIO Seq.empty
  active <- newTVarIO 0
  waiters <- newTVarIO Seq.empty
  closed <- newTVarIO False
  reaperRef <- newIORef (error "reaper not started")
  warmerRef <- newIORef Nothing
  effectiveSize <- newTVarIO (poolSize cfg)
  totalCreated <- newTVarIO 0
  totalDestroyed <- newTVarIO 0
  totalTimeouts <- newTVarIO 0
  connMeta <- newTVarIO Map.empty
  onCreate <- newIORef (\_ -> pure ())
  onAcquire <- newIORef (\_ -> pure ())
  onRelease <- newIORef (\_ -> pure ())
  typeCache <- newTypeCache
  let pool =
        Pool
          { pConfig = cfg
          , pIdle = idle
          , pActive = active
          , pWaiters = waiters
          , pClosed = closed
          , pReaper = reaperRef
          , pWarmer = warmerRef
          , pEffectiveSize = effectiveSize
          , pTotalCreated = totalCreated
          , pTotalDestroyed = totalDestroyed
          , pTotalTimeouts = totalTimeouts
          , pConnMeta = connMeta
          , pOnCreate = onCreate
          , pOnAcquire = onAcquire
          , pOnRelease = onRelease
          , pTypeCache = typeCache
          }
  reaper <- async (reaperThread pool)
  writeIORef reaperRef reaper
  -- Spawn warmer if poolMinIdle > 0
  when (poolMinIdle cfg > 0) $ do
    w <- async (warmerThread pool)
    writeIORef warmerRef (Just w)
  pure pool

-- | Close the pool, releasing all resources.
--
-- Closes every idle connection, cancels background threads (reaper and warmer),
-- and wakes all blocked waiters with a 'PoolClosed' error. After this call,
-- any subsequent 'withResource' will throw 'PoolClosed'. Idempotent: calling
-- 'closePool' on an already-closed pool is a no-op for the connection teardown.
closePool :: Pool -> IO ()
closePool pool = do
  -- Atomically: mark closed, drain idle, drain waiters
  (entries, blockedWaiters) <- atomically $ do
    writeTVar (pClosed pool) True
    idle <- readTVar (pIdle pool)
    writeTVar (pIdle pool) Seq.empty
    ws <- readTVar (pWaiters pool)
    writeTVar (pWaiters pool) Seq.empty
    pure (idle, ws)
  -- Cancel background threads
  reaper <- readIORef (pReaper pool)
  cancel reaper
  mWarmer <- readIORef (pWarmer pool)
  mapM_ cancel mWarmer
  -- Wake all blocked waiters with PoolClosed
  mapM_ (\w -> atomically $ tryPutTMVar w (Left PoolClosed)) blockedWaiters
  -- Close all idle connections (properly tracked)
  mapM_ (destroyEntry pool "pool closed") entries
  observe pool PoolShutdown

-- | Gracefully drain the pool for rolling deployments.
--
-- Marks the pool as closed (no new acquires), then polls until all
-- in-use connections are returned, then closes everything. Blocks
-- until the drain is complete or the timeout expires.
--
-- @
-- -- During rolling deployment:
-- drainPool pool 30  -- wait up to 30 seconds for in-flight queries
-- @
drainPool :: Pool -> NominalDiffTime -> IO ()
drainPool pool timeout = do
  -- Mark as closed — new withResource calls will get PoolClosed
  atomically $ writeTVar (pClosed pool) True
  -- Wake blocked waiters immediately
  ws <- atomically $ do
    waiters <- readTVar (pWaiters pool)
    writeTVar (pWaiters pool) Seq.empty
    pure waiters
  mapM_ (\w -> atomically $ tryPutTMVar w (Left PoolClosed)) ws
  -- Poll until all active connections are returned or timeout
  let timeoutMicros = round (timeout * 1000000) :: Int
      pollInterval = 100000 -- 100ms
      pollLoop !remaining
        | remaining <= 0 = pure () -- timeout, force close
        | otherwise = do
            active <- readTVarIO (pActive pool)
            idle <- Seq.length <$> readTVarIO (pIdle pool)
            if active <= idle
              then pure () -- all connections returned to idle
              else do
                threadDelay pollInterval
                pollLoop (remaining - pollInterval)
  pollLoop timeoutMicros
  -- Now close everything (reuse closePool's cleanup logic)
  entries <- atomically $ do
    idle <- readTVar (pIdle pool)
    writeTVar (pIdle pool) Seq.empty
    pure idle
  reaper <- readIORef (pReaper pool)
  cancel reaper
  mWarmer <- readIORef (pWarmer pool)
  mapM_ cancel mWarmer
  mapM_ (destroyEntry pool "drain") entries
  observe pool PoolShutdown

-- | Acquire a connection from the pool, run an action, and return the connection.
--
-- The connection is guaranteed to be returned to the pool (or destroyed) even
-- if the action throws an exception. Uses 'mask' internally so the acquire and
-- release cannot be interrupted by async exceptions.
--
-- @
-- withResource pool $ \\conn ->
--   simpleQuery conn \"SELECT 1\"
-- @
withResource :: Pool -> (Connection -> IO a) -> IO a
withResource pool action = mask $ \restore -> do
  t0 <- getCurrentTime
  conn <- acquire pool
  t1 <- getCurrentTime
  observe pool (ConnectionAcquired (diffUTCTime t1 t0))
  result <- restore (action conn) `onException` destroyConn pool conn "exception"
  release pool conn
  observe pool ConnectionReleased
  pure result

-- | Like 'withResource' but with a custom acquire timeout.
--
-- Overrides 'poolAcquireTimeout' for this single acquisition. Useful when
-- certain operations can tolerate longer (or shorter) waits than the pool
-- default.
--
-- @
-- -- Wait up to 30 seconds for this particular query
-- withResourceTimeout pool 30 $ \\conn ->
--   simpleQuery conn \"SELECT expensive_function()\"
-- @
withResourceTimeout :: Pool -> NominalDiffTime -> (Connection -> IO a) -> IO a
withResourceTimeout pool timeout action = mask $ \restore -> do
  t0 <- getCurrentTime
  conn <- acquireWithTimeout pool timeout
  t1 <- getCurrentTime
  observe pool (ConnectionAcquired (diffUTCTime t1 t0))
  result <- restore (action conn) `onException` destroyConn pool conn "exception"
  release pool conn
  observe pool ConnectionReleased
  pure result

-- | Get an atomic snapshot of the pool's current statistics.
--
-- The returned 'PoolStats' is consistent (all fields read in a single STM
-- transaction) but represents a point-in-time view that may be stale by
-- the time you inspect it.
poolStats :: Pool -> IO PoolStats
poolStats pool = atomically $ do
  idle <- Seq.length <$> readTVar (pIdle pool)
  active <- readTVar (pActive pool)
  waiters <- Seq.length <$> readTVar (pWaiters pool)
  maxSz <- readTVar (pEffectiveSize pool)
  created <- readTVar (pTotalCreated pool)
  destroyed <- readTVar (pTotalDestroyed pool)
  timeouts <- readTVar (pTotalTimeouts pool)
  pure PoolStats
    { psIdle = idle
    , psInUse = active - idle
    , psWaiters = waiters
    , psMaxSize = maxSz
    , psTotalCreated = created
    , psTotalDestroyed = destroyed
    , psTotalTimeouts = timeouts
    }

-- | Check if the pool is still open (not closed).
--
-- Returns 'True' if the pool is alive and accepting connections,
-- 'False' if 'closePool' has been called.
poolIsAlive :: Pool -> IO Bool
poolIsAlive pool = not <$> readTVarIO (pClosed pool)

-- | Resize the pool at runtime to a new maximum connection count.
--
-- If shrinking, excess idle connections are closed immediately. In-use
-- connections are not interrupted and will finish naturally; they are simply
-- not returned to the pool once the count exceeds the new limit. If growing,
-- new connections are created on demand by subsequent 'withResource' calls.
resize :: Pool -> Int -> IO ()
resize pool newSize = do
  (oldSize, excess) <- atomically $ do
    old <- readTVar (pEffectiveSize pool)
    writeTVar (pEffectiveSize pool) newSize
    idle <- readTVar (pIdle pool)
    let idleCount = Seq.length idle
        excessCount = max 0 (idleCount - newSize)
    if excessCount > 0
      then do
        let (keep, toDrop) = Seq.splitAt (idleCount - excessCount) idle
        writeTVar (pIdle pool) keep
        pure (old, seqToList toDrop)
      else pure (old, [])
  logPool pool "info" ("resized from " <> BS8.pack (show oldSize) <> " to " <> BS8.pack (show newSize))
  observe pool (PoolResized oldSize newSize)
  mapM_ (destroyEntry pool "resize") excess

-- | Filter idle connections, keeping only those that satisfy the predicate.
--
-- Atomically drains the idle queue, applies the predicate to each connection
-- in IO, returns matching ones to the pool, and destroys the rest. Useful for
-- invalidating connections after a credential rotation or schema migration.
--
-- @
-- -- Drop all connections that were created before the migration
-- retain pool (\\conn -> isPostMigration conn)
-- @
retain :: Pool -> (Connection -> IO Bool) -> IO ()
retain pool predicate = do
  entries <- atomically $ do
    idle <- readTVar (pIdle pool)
    writeTVar (pIdle pool) Seq.empty
    pure (seqToList idle)
  (kept, dropped) <- partitionM (\e -> predicate (peConn e)) entries
  atomically $ modifyTVar' (pIdle pool) (Seq.fromList kept Seq.><)
  mapM_ (destroyEntry pool "retain") dropped

-- | Set a hook called immediately after a new connection is created.
--
-- Useful for running session-level setup such as @SET search_path@ or
-- @SET statement_timeout@. If the hook throws, the exception is silently
-- caught and the connection is still placed in the pool.
setPostCreateHook :: Pool -> (Connection -> IO ()) -> IO ()
setPostCreateHook pool = writeIORef (pOnCreate pool)

-- | Set a hook called each time a connection is handed out to a caller via 'withResource'.
--
-- Runs after any recycling\/health checks have passed. If the hook throws, the
-- exception is silently caught and the connection is still provided.
setOnAcquireHook :: Pool -> (Connection -> IO ()) -> IO ()
setOnAcquireHook pool = writeIORef (pOnAcquire pool)

-- | Set a hook called before a connection is returned to the idle pool.
--
-- Runs after the caller's action completes and before the connection is placed
-- back in the idle queue or delivered to a waiting thread. Useful for cleanup
-- such as @RESET ALL@ or @DISCARD TEMP@. If the hook throws, the exception is
-- silently caught.
setPreReleaseHook :: Pool -> (Connection -> IO ()) -> IO ()
setPreReleaseHook pool = writeIORef (pOnRelease pool)

-- Internal ----------------------------------------------------------------

-- | What the single STM transaction decided we should do.
data AcquireAction
  = GotIdle !PoolEntry
  | CreateNew
  | MustWait !(TMVar (Either HsqlxError Connection))
  | PoolIsClosed

-- | Atomically decide what to do: take idle, create new, or enqueue waiter.
-- This is the core fix for the acquire race condition — a single STM
-- transaction replaces the old two-step check.
acquireAction :: Pool -> STM AcquireAction
acquireAction pool = do
  closed <- readTVar (pClosed pool)
  if closed
    then pure PoolIsClosed
    else do
      idle <- readTVar (pIdle pool)
      case takeIdle (poolQueueMode (pConfig pool)) idle of
        Just (entry, rest) -> do
          writeTVar (pIdle pool) rest
          pure (GotIdle entry)
        Nothing -> do
          active <- readTVar (pActive pool)
          maxSz <- readTVar (pEffectiveSize pool)
          if active < maxSz
            then do
              writeTVar (pActive pool) (active + 1)
              pure CreateNew
            else do
              waiter <- newEmptyTMVar
              modifyTVar' (pWaiters pool) (Seq.|> waiter)
              pure (MustWait waiter)

-- | Take from the appropriate end based on QueueMode.
takeIdle :: QueueMode -> Seq PoolEntry -> Maybe (PoolEntry, Seq PoolEntry)
takeIdle QueueLIFO s = case Seq.viewr s of
  Seq.EmptyR -> Nothing
  rest Seq.:> e -> Just (e, rest)
takeIdle QueueFIFO s = case Seq.viewl s of
  Seq.EmptyL -> Nothing
  e Seq.:< rest -> Just (e, rest)

acquire :: Pool -> IO Connection
acquire pool = acquireWithTimeout pool (poolAcquireTimeout (pConfig pool))

acquireWithTimeout :: Pool -> NominalDiffTime -> IO Connection
acquireWithTimeout pool timeout = do
  act <- atomically (acquireAction pool)
  case act of
    PoolIsClosed -> throwHsqlx PoolClosed
    GotIdle entry -> tryRecycle pool entry
    CreateNew -> createConnection pool
    MustWait waiter -> waitForConnectionWith pool waiter timeout

-- | Try to recycle an idle connection. If it's expired or unhealthy,
-- destroy it and retry.
tryRecycle :: Pool -> PoolEntry -> IO Connection
tryRecycle pool entry = do
  now <- getCurrentTime
  expired <- isExpiredIO pool entry now
  if expired
    then do
      destroyEntry pool "expired" entry
      acquire pool
    else do
      ok <- recycleCheck pool entry now
      if ok
        then do
          logPool pool "debug" "recycled connection"
          observe pool ConnectionRecycled
          writeIORef (peLastUsed entry) now
          runHook (pOnAcquire pool) (peConn entry)
          pure (peConn entry)
        else do
          observe pool HealthCheckFailed
          destroyEntry pool "unhealthy" entry
          acquire pool

-- | Recycling check based on the configured method.
recycleCheck :: Pool -> PoolEntry -> UTCTime -> IO Bool
recycleCheck pool entry now = case poolRecyclingMethod (pConfig pool) of
  RecycleFast -> do
    readTVarIO (awcAlive (connAsync (peConn entry)))
  RecycleVerified -> do
    alive <- readTVarIO (awcAlive (connAsync (peConn entry)))
    if not alive
      then pure False
      else do
        lastUsed <- readIORef (peLastUsed entry)
        if diffUTCTime now lastUsed <= poolHealthCheckAge (pConfig pool)
          then pure True
          else checkHealthWithTimeout (poolAcquireTimeout (pConfig pool)) (peConn entry)
  RecycleClean -> do
    alive <- readTVarIO (awcAlive (connAsync (peConn entry)))
    if not alive
      then pure False
      else do
        let timeoutSecs = poolAcquireTimeout (pConfig pool)
            timeoutMicros = round (timeoutSecs * 1000000) :: Int
        result <- race (threadDelay timeoutMicros)
                       (try @SomeException (simpleQuery (peConn entry) "DISCARD ALL"))
        case result of
          Right (Right _) -> do
            -- DISCARD ALL deallocates all prepared statements on the server,
            -- so the client-side cache must be cleared to stay in sync.
            writeIORef (connStmtCache (peConn entry)) PSQ.empty
            pure True
          _ -> pure False

createConnection :: Pool -> IO Connection
createConnection pool = do
  conn <- connectWithRetry (poolConnectionRetries (pConfig pool)) 100000
    `onException` atomically (modifyTVar' (pActive pool) (subtract 1))
  now <- getCurrentTime
  deadline <- jitteredDeadline (pConfig pool) now
  atomically $ do
    modifyTVar' (pTotalCreated pool) (+ 1)
    modifyTVar' (pConnMeta pool) (Map.insert (connBackendPid conn) (ConnMeta now deadline))
  logPool pool "debug" "created connection"
  observe pool ConnectionCreated
  runHook (pOnCreate pool) conn
  runHook (pOnAcquire pool) conn
  pure conn
  where
    -- Exponential backoff: 100ms, 200ms, 400ms, ...
    connectWithRetry :: Int -> Int -> IO Connection
    connectWithRetry 0 _ = connectString (poolConnString (pConfig pool))
    connectWithRetry !retriesLeft !delayMicros = do
      result <- try @SomeException (connectString (poolConnString (pConfig pool)))
      case result of
        Right c -> pure c
        Left _ -> do
          logPool pool "warn" ("connection failed, retrying in " <> BS8.pack (show (delayMicros `div` 1000)) <> "ms")
          threadDelay delayMicros
          connectWithRetry (retriesLeft - 1) (delayMicros * 2)

-- | Compute a jittered deadline for a connection.
-- deadline = now + maxLife + uniform(-jitter, +jitter)
jitteredDeadline :: PoolConfig -> UTCTime -> IO UTCTime
jitteredDeadline cfg now = do
  let jitter = poolMaxLifeJitter cfg
  offset <- if jitter > 0
    then do
      let jitterMicros = round (jitter * 1000000) :: Int
      r <- randomRIO (negate jitterMicros, jitterMicros)
      pure (fromIntegral r / 1000000 :: NominalDiffTime)
    else pure 0
  pure (addUTCTime (poolMaxLife cfg + offset) now)

waitForConnectionWith :: Pool -> TMVar (Either HsqlxError Connection) -> NominalDiffTime -> IO Connection
waitForConnectionWith pool waiter timeout = do
  let timeoutMicros = round (timeout * 1000000) :: Int
  result <- race
    (threadDelay timeoutMicros)
    (atomically $ takeTMVar waiter)
  case result of
    Left () -> do
      atomically $ modifyTVar' (pTotalTimeouts pool) (+ 1)
      logPool pool "warn" "acquire timeout"
      observe pool AcquireTimeout
      throwHsqlx PoolTimeout
    Right (Left err) -> throwHsqlx err
    Right (Right conn) -> do
      runHook (pOnAcquire pool) conn
      pure conn

release :: Pool -> Connection -> IO ()
release pool conn = do
  runHook (pOnRelease pool) conn
  now <- getCurrentTime
  -- Try to deliver to a waiter first. Use tryPutTMVar and loop
  -- to handle dead waiters (waiter died between enqueue and delivery).
  delivered <- deliverToWaiter pool conn
  if delivered
    then pure ()
    else do
      -- Look up creation time from the tracking map (fixes maxLife bug)
      meta <- Map.lookup (connBackendPid conn) <$> readTVarIO (pConnMeta pool)
      let createdAt = case meta of
            Just cm -> cmCreatedAt cm
            Nothing -> now
      lastUsed <- newIORef now
      let entry = PoolEntry conn createdAt lastUsed
      atomically $ modifyTVar' (pIdle pool) (Seq.|> entry)

-- | Try to deliver a connection to a waiting thread. Loops through waiters,
-- skipping any that have died (timed out). Returns True if delivered.
deliverToWaiter :: Pool -> Connection -> IO Bool
deliverToWaiter pool conn = atomically $ do
  ws <- readTVar (pWaiters pool)
  case Seq.viewl ws of
    Seq.EmptyL -> pure False
    w Seq.:< rest -> do
      delivered <- tryPutTMVar w (Right conn)
      if delivered
        then do
          writeTVar (pWaiters pool) rest
          pure True
        else do
          -- Waiter died (timeout). Skip it and try next.
          writeTVar (pWaiters pool) rest
          -- Retry with remaining waiters (re-enter STM)
          deliverLoop pool rest conn

-- | STM loop to deliver to next available waiter.
deliverLoop :: Pool -> Seq (TMVar (Either HsqlxError Connection)) -> Connection -> STM Bool
deliverLoop _ Empty _ = pure False
deliverLoop pool (w :<| rest) conn = do
  delivered <- tryPutTMVar w (Right conn)
  if delivered
    then do
      writeTVar (pWaiters pool) rest
      pure True
    else do
      writeTVar (pWaiters pool) rest
      deliverLoop pool rest conn

destroyConn :: Pool -> Connection -> BS8.ByteString -> IO ()
destroyConn pool conn reason = do
  safeClose conn
  atomically $ do
    modifyTVar' (pActive pool) (subtract 1)
    modifyTVar' (pTotalDestroyed pool) (+ 1)
    modifyTVar' (pConnMeta pool) (Map.delete (connBackendPid conn))
  logPool pool "debug" ("destroyed connection: " <> reason)
  observe pool (ConnectionDestroyed reason)

destroyEntry :: Pool -> BS8.ByteString -> PoolEntry -> IO ()
destroyEntry pool reason entry = destroyConn pool (peConn entry) reason

-- | Check if a connection has exceeded its (jittered) max-life.
isExpiredIO :: Pool -> PoolEntry -> UTCTime -> IO Bool
isExpiredIO pool entry now = do
  meta <- Map.lookup (connBackendPid (peConn entry)) <$> readTVarIO (pConnMeta pool)
  pure $ case meta of
    Just cm -> now >= cmDeadline cm
    Nothing -> diffUTCTime now (peCreatedAt entry) > poolMaxLife (pConfig pool)

safeClose :: Connection -> IO ()
safeClose conn = close conn `catch` \(_ :: SomeException) -> pure ()

-- | Lightweight health check: send an empty query and see if we get a response.
-- Times out after the given duration to prevent a hung backend from blocking acquire.
checkHealthWithTimeout :: NominalDiffTime -> Connection -> IO Bool
checkHealthWithTimeout timeout conn = do
  let timeoutMicros = round (timeout * 1000000) :: Int
  result <- race (threadDelay timeoutMicros) (try @SomeException (simpleQuery conn ""))
  pure $ case result of
    Left () -> False    -- timed out
    Right (Right _) -> True
    Right (Left _) -> False

-- | Run a lifecycle hook, catching and ignoring exceptions so a hook
-- failure doesn't break the pool.
runHook :: IORef (Connection -> IO ()) -> Connection -> IO ()
runHook ref conn = do
  hook <- readIORef ref
  hook conn `catch` \(_ :: SomeException) -> pure ()

-- | Log a pool event using the configured logger.
logPool :: Pool -> BS8.ByteString -> BS8.ByteString -> IO ()
logPool pool = poolLogger (pConfig pool)

-- | Emit a structured observation event, catching and ignoring exceptions.
observe :: Pool -> PoolEvent -> IO ()
observe pool event =
  poolObserver (pConfig pool) event `catch` \(_ :: SomeException) -> pure ()

-- | Background thread that periodically reaps idle/expired connections.
--
-- Atomically drains the idle queue, filters in IO (reading IORefs),
-- then atomically merges kept entries back. This avoids the race where
-- acquire/release modify the idle queue between read and write.
reaperThread :: Pool -> IO ()
reaperThread pool = go
  where
    intervalMicros = round (poolReaperInterval (pConfig pool) * 1000000) :: Int

    go = do
      threadDelay intervalMicros
      reap
        `catch` \(e :: AsyncException) -> throwIO e
        `catch` \(_ :: SomeException) ->
          logPool pool "warn" "reaper: exception in sweep cycle (continuing)"
      go

    reap = do
      now <- getCurrentTime
      -- Atomically drain the idle queue
      entries <- atomically $ do
        idle <- readTVar (pIdle pool)
        writeTVar (pIdle pool) Seq.empty
        pure (seqToList idle)
      (keep, toReap) <- partitionM (shouldKeep now) entries
      -- Merge kept entries back (prepend; entries added by release
      -- during filtering are already at the tail)
      atomically $ modifyTVar' (pIdle pool) (Seq.fromList keep Seq.><)
      let reapCount = length toReap
      mapM_ (destroyEntry pool "reaped") toReap
      when (reapCount > 0) $ do
        logPool pool "info" ("reaper swept " <> BS8.pack (show reapCount) <> " connections")
        observe pool (ReaperSwept reapCount)

    shouldKeep now entry = do
      expired <- isExpiredIO pool entry now
      if expired
        then pure False
        else do
          lastUsed <- readIORef (peLastUsed entry)
          pure (diffUTCTime now lastUsed <= poolIdleTime (pConfig pool))

-- | Background thread that maintains 'poolMinIdle' connections.
warmerThread :: Pool -> IO ()
warmerThread pool = go
  where
    intervalMicros = round (poolReaperInterval (pConfig pool) * 1000000) :: Int

    go = do
      threadDelay intervalMicros
      warm
        `catch` \(e :: AsyncException) -> throwIO e
        `catch` \(_ :: SomeException) ->
          logPool pool "warn" "warmer: exception in warm cycle (continuing)"
      go

    warm = do
      deficit <- atomically $ do
        closed <- readTVar (pClosed pool)
        if closed
          then pure 0
          else do
            idle <- Seq.length <$> readTVar (pIdle pool)
            active <- readTVar (pActive pool)
            maxSz <- readTVar (pEffectiveSize pool)
            let minIdle = poolMinIdle (pConfig pool)
                canCreate = maxSz - active
                need = max 0 (minIdle - idle)
            pure (min need canCreate)
      created <- warmN 0 deficit
      when (created > 0) $ do
        logPool pool "info" ("warmer created " <> BS8.pack (show created) <> " connections")
        observe pool (WarmerCreated created)

    warmN :: Int -> Int -> IO Int
    warmN !acc 0 = pure acc
    warmN !acc n = do
      result <- try @SomeException $ do
        -- Reserve a slot
        reserved <- atomically $ do
          active <- readTVar (pActive pool)
          maxSz <- readTVar (pEffectiveSize pool)
          if active < maxSz
            then do
              writeTVar (pActive pool) (active + 1)
              pure True
            else pure False
        if reserved
          then do
            conn <- connectString (poolConnString (pConfig pool))
              `onException` atomically (modifyTVar' (pActive pool) (subtract 1))
            now <- getCurrentTime
            deadline <- jitteredDeadline (pConfig pool) now
            atomically $ do
              modifyTVar' (pTotalCreated pool) (+ 1)
              modifyTVar' (pConnMeta pool) (Map.insert (connBackendPid conn) (ConnMeta now deadline))
            runHook (pOnCreate pool) conn
            lastUsed <- newIORef now
            let entry = PoolEntry conn now lastUsed
            atomically $ modifyTVar' (pIdle pool) (Seq.|> entry)
            pure True
          else pure False
      case result of
        Left _ -> pure acc -- Connection failed, stop warming this round
        Right True -> warmN (acc + 1) (n - 1)
        Right False -> pure acc

when :: Bool -> IO () -> IO ()
when True f = f
when False _ = pure ()

seqToList :: Seq a -> [a]
seqToList = foldr (:) []

partitionM :: (a -> IO Bool) -> [a] -> IO ([a], [a])
partitionM _ [] = pure ([], [])
partitionM p (x : xs) = do
  b <- p x
  (!ys, !zs) <- partitionM p xs
  pure (if b then (x : ys, zs) else (ys, x : zs))

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
  , withResource
  , poolStats
  , resize
  , retain
  , setPostCreateHook
  , setOnAcquireHook
  , setPreReleaseHook
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, mask, onException, try)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import PgWire.Async (AsyncWireConn (..))
import PgWire.Connection (Connection (..), close, connectString, simpleQuery)
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Pool.Config (PoolConfig (..), QueueMode (..), RecyclingMethod (..))
import PgWire.TypeCache (TypeCache, newTypeCache)
import System.Random (randomRIO)

-- | A connection pool.
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

data PoolEntry = PoolEntry
  { peConn :: Connection
  , peCreatedAt :: UTCTime
  , peLastUsed :: IORef UTCTime
  }

-- | Snapshot of pool statistics.
data PoolStats = PoolStats
  { psIdle :: !Int
  , psInUse :: !Int
  , psWaiters :: !Int
  , psMaxSize :: !Int
  , psTotalCreated :: !Int
  , psTotalDestroyed :: !Int
  , psTotalTimeouts :: !Int
  }
  deriving stock (Show, Eq)

-- | Create a new connection pool.
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

-- | Close the pool and all connections.
-- Wakes all blocked waiters with 'PoolClosed'.
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

-- | Acquire a connection, run an action, and return the connection.
withResource :: Pool -> (Connection -> IO a) -> IO a
withResource pool action = mask $ \restore -> do
  conn <- acquire pool
  result <- restore (action conn) `onException` destroyConn pool conn "exception"
  release pool conn
  pure result

-- | Get a snapshot of the pool's statistics.
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

-- | Resize the pool at runtime. If shrinking, excess idle connections are
-- closed immediately. In-use connections finish naturally.
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
  mapM_ (destroyEntry pool "resize") excess

-- | Filter idle connections. Pulls all idle connections, applies the
-- predicate, returns matching ones to the pool, destroys the rest.
-- Useful for connection string rotation or schema changes.
retain :: Pool -> (Connection -> IO Bool) -> IO ()
retain pool predicate = do
  entries <- atomically $ do
    idle <- readTVar (pIdle pool)
    writeTVar (pIdle pool) Seq.empty
    pure (seqToList idle)
  (kept, dropped) <- partitionM (\e -> predicate (peConn e)) entries
  atomically $ modifyTVar' (pIdle pool) (Seq.fromList kept Seq.><)
  mapM_ (destroyEntry pool "retain") dropped

-- | Set a hook called after a new connection is created.
setPostCreateHook :: Pool -> (Connection -> IO ()) -> IO ()
setPostCreateHook pool = writeIORef (pOnCreate pool)

-- | Set a hook called when a connection is handed to the caller.
setOnAcquireHook :: Pool -> (Connection -> IO ()) -> IO ()
setOnAcquireHook pool = writeIORef (pOnAcquire pool)

-- | Set a hook called before a connection is returned to the idle pool.
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
acquire pool = do
  act <- atomically (acquireAction pool)
  case act of
    PoolIsClosed -> throwHsqlx PoolClosed
    GotIdle entry -> tryRecycle pool entry
    CreateNew -> createConnection pool
    MustWait waiter -> waitForConnection pool waiter

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
          writeIORef (peLastUsed entry) now
          runHook (pOnAcquire pool) (peConn entry)
          pure (peConn entry)
        else do
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
          else checkHealth (peConn entry)
  RecycleClean -> do
    alive <- readTVarIO (awcAlive (connAsync (peConn entry)))
    if not alive
      then pure False
      else do
        result <- try @SomeException (simpleQuery (peConn entry) "DISCARD ALL")
        pure $ case result of
          Right _ -> True
          Left _ -> False

createConnection :: Pool -> IO Connection
createConnection pool = do
  conn <-
    connectString (poolConnString (pConfig pool))
      `onException` atomically (modifyTVar' (pActive pool) (subtract 1))
  now <- getCurrentTime
  deadline <- jitteredDeadline (pConfig pool) now
  atomically $ do
    modifyTVar' (pTotalCreated pool) (+ 1)
    modifyTVar' (pConnMeta pool) (Map.insert (connBackendPid conn) (ConnMeta now deadline))
  logPool pool "debug" "created connection"
  runHook (pOnCreate pool) conn
  runHook (pOnAcquire pool) conn
  pure conn

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

waitForConnection :: Pool -> TMVar (Either HsqlxError Connection) -> IO Connection
waitForConnection pool waiter = do
  let timeoutMicros = round (poolAcquireTimeout (pConfig pool) * 1000000) :: Int
  result <- race
    (threadDelay timeoutMicros)
    (atomically $ takeTMVar waiter)
  case result of
    Left () -> do
      atomically $ modifyTVar' (pTotalTimeouts pool) (+ 1)
      logPool pool "warn" "acquire timeout"
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
checkHealth :: Connection -> IO Bool
checkHealth conn = do
  result <- try @SomeException (simpleQuery conn "")
  pure $ case result of
    Right _ -> True
    Left _ -> False

-- | Run a lifecycle hook, catching and ignoring exceptions so a hook
-- failure doesn't break the pool.
runHook :: IORef (Connection -> IO ()) -> Connection -> IO ()
runHook ref conn = do
  hook <- readIORef ref
  hook conn `catch` \(_ :: SomeException) -> pure ()

-- | Log a pool event using the configured logger.
logPool :: Pool -> BS8.ByteString -> BS8.ByteString -> IO ()
logPool pool level msg = poolLogger (pConfig pool) level msg

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
      now <- getCurrentTime
      -- Atomically drain the idle queue
      entries <- atomically $ do
        idle <- readTVar (pIdle pool)
        writeTVar (pIdle pool) Seq.empty
        pure (seqToList idle)
      (keep, reap) <- partitionM (shouldKeep now) entries
      -- Merge kept entries back (prepend; entries added by release
      -- during filtering are already at the tail)
      atomically $ modifyTVar' (pIdle pool) (Seq.fromList keep Seq.><)
      let reapCount = length reap
      mapM_ (destroyEntry pool "reaped") reap
      when (reapCount > 0) $
        logPool pool "info" ("reaper swept " <> BS8.pack (show reapCount) <> " connections")
      go

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
      when (created > 0) $
        logPool pool "info" ("warmer created " <> BS8.pack (show created) <> " connections")
      go

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
  (ys, zs) <- partitionM p xs
  pure (if b then (x : ys, zs) else (ys, x : zs))

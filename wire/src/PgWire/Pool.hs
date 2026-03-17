-- | Thread-safe connection pool for PostgreSQL.
--
-- Manages a set of reusable 'Connection's with configurable pool size,
-- idle timeout, max lifetime, and health checking. Connections are
-- validated with an empty query before reuse.
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
  , newPool
  , closePool
  , withResource
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, mask, onException, try)
import Data.ByteString (ByteString)
import Data.Hashable (hash)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import PgWire.Connection (Connection (..), close, connectString, simpleQuery)
import PgWire.Error (HsqlxError (..), throwHsqlx)
import PgWire.Pool.Config (PoolConfig (..))

-- | A connection pool.
data Pool = Pool
  { pConfig :: PoolConfig
  , pIdle :: TVar [PoolEntry]
  , pActive :: TVar Int
  , pWaiters :: TVar (Seq (TMVar Connection))
  , pClosed :: TVar Bool
  , pReaper :: IORef (Async ())
  , pSharedStmts :: TVar (Map Int ByteString)
  -- ^ Shared statement cache: hash(SQL) → server-side statement name.
  -- When connection A prepares a statement, it registers here.
  -- Connection B can skip Parse if the name is already registered.
  }

data PoolEntry = PoolEntry
  { peConn :: Connection
  , peCreatedAt :: UTCTime
  , peLastUsed :: IORef UTCTime
  }

-- | Create a new connection pool.
newPool :: PoolConfig -> IO Pool
newPool cfg = do
  idle <- newTVarIO []
  active <- newTVarIO 0
  waiters <- newTVarIO Seq.empty
  closed <- newTVarIO False
  reaperRef <- newIORef (error "reaper not started")
  sharedStmts <- newTVarIO Map.empty
  let pool =
        Pool
          { pConfig = cfg
          , pIdle = idle
          , pActive = active
          , pWaiters = waiters
          , pClosed = closed
          , pReaper = reaperRef
          , pSharedStmts = sharedStmts
          }
  reaper <- async (reaperThread pool)
  writeIORef reaperRef reaper
  pure pool

-- | Close the pool and all connections.
closePool :: Pool -> IO ()
closePool pool = do
  atomically $ writeTVar (pClosed pool) True
  reaper <- readIORef (pReaper pool)
  cancel reaper
  entries <- atomically $ do
    idle <- readTVar (pIdle pool)
    writeTVar (pIdle pool) []
    pure idle
  mapM_ (safeClose . peConn) entries

-- | Acquire a connection, run an action, and return the connection.
withResource :: Pool -> (Connection -> IO a) -> IO a
withResource pool action = mask $ \restore -> do
  conn <- acquire pool
  result <- restore (action conn) `onException` destroyConn pool conn
  release pool conn
  pure result

-- Internal ----------------------------------------------------------------

acquire :: Pool -> IO Connection
acquire pool = do
  isClosed <- atomically $ readTVar (pClosed pool)
  if isClosed
    then throwHsqlx PoolClosed
    else do
      -- Try to take from idle pool
      mEntry <- atomically $ do
        idle <- readTVar (pIdle pool)
        case idle of
          (e : es) -> do
            writeTVar (pIdle pool) es
            pure (Just e)
          [] -> pure Nothing
      case mEntry of
        Just entry -> do
          now <- getCurrentTime
          if isExpired (pConfig pool) entry now
            then do
              safeClose (peConn entry)
              atomically $ modifyTVar' (pActive pool) (subtract 1)
              acquire pool -- retry
            else do
              -- Health check: verify the connection is still alive
              healthy <- checkHealth (peConn entry)
              if healthy
                then do
                  writeIORef (peLastUsed entry) now
                  pure (peConn entry)
                else do
                  safeClose (peConn entry)
                  atomically $ modifyTVar' (pActive pool) (subtract 1)
                  acquire pool -- retry
        Nothing -> do
          -- Try to create a new connection
          canCreate <- atomically $ do
            active <- readTVar (pActive pool)
            if active < poolSize (pConfig pool)
              then do
                writeTVar (pActive pool) (active + 1)
                pure True
              else pure False
          if canCreate
            then do
              conn <-
                connectString (poolConnString (pConfig pool))
                  `onException` atomically (modifyTVar' (pActive pool) (subtract 1))
              pure conn
            else do
              -- Wait for a connection to become available, with timeout
              waiter <- newEmptyTMVarIO
              atomically $ modifyTVar' (pWaiters pool) (Seq.|> waiter)
              let timeoutMicros = round (poolAcquireTimeout (pConfig pool) * 1000000) :: Int
              result <- race
                (threadDelay timeoutMicros)
                (atomically $ takeTMVar waiter)
              case result of
                Left () -> throwHsqlx PoolTimeout
                Right conn -> pure conn

release :: Pool -> Connection -> IO ()
release pool conn = do
  now <- getCurrentTime
  -- Check if any waiters are waiting
  mWaiter <- atomically $ do
    waiters <- readTVar (pWaiters pool)
    case Seq.viewl waiters of
      Seq.EmptyL -> pure Nothing
      w Seq.:< ws -> do
        writeTVar (pWaiters pool) ws
        pure (Just w)
  case mWaiter of
    Just waiter -> atomically $ putTMVar waiter conn
    Nothing -> do
      lastUsed <- newIORef now
      let entry = PoolEntry conn now lastUsed
      atomically $ modifyTVar' (pIdle pool) (entry :)

destroyConn :: Pool -> Connection -> IO ()
destroyConn pool conn = do
  safeClose conn
  atomically $ modifyTVar' (pActive pool) (subtract 1)

isExpired :: PoolConfig -> PoolEntry -> UTCTime -> Bool
isExpired cfg entry now =
  diffUTCTime now (peCreatedAt entry) > poolMaxLife cfg

safeClose :: Connection -> IO ()
safeClose conn = close conn `catch` \(_ :: SomeException) -> pure ()

-- | Lightweight health check: send an empty query and see if we get a response.
-- Returns False if the connection is dead.
checkHealth :: Connection -> IO Bool
checkHealth conn = do
  result <- try @SomeException (simpleQuery conn "")
  pure $ case result of
    Right _ -> True
    Left _ -> False

-- | Background thread that periodically reaps idle/expired connections.
reaperThread :: Pool -> IO ()
reaperThread pool = go
  where
    go = do
      threadDelay (30 * 1000000) -- check every 30 seconds
      now <- getCurrentTime
      -- Read all idle entries and check which to reap (in IO, not STM)
      entries <- atomically $ readTVar (pIdle pool)
      (keep, reap) <- partitionM (shouldKeep now) entries
      atomically $ writeTVar (pIdle pool) keep
      mapM_ (\e -> safeClose (peConn e) >> atomically (modifyTVar' (pActive pool) (subtract 1))) reap
      go

    shouldKeep now entry = do
      if isExpired (pConfig pool) entry now
        then pure False
        else do
          lastUsed <- readIORef (peLastUsed entry)
          pure (diffUTCTime now lastUsed <= poolIdleTime (pConfig pool))

    partitionM _ [] = pure ([], [])
    partitionM p (x : xs) = do
      b <- p x
      (ys, zs) <- partitionM p xs
      pure (if b then (x : ys, zs) else (ys, x : zs))



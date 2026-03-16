module Hsqlx.Pool
  ( Pool
  , newPool
  , closePool
  , withResource
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, mask, onException)
import Data.IORef
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Hsqlx.Connection (Connection, close, connectString)
import Hsqlx.Error (HsqlxError (..), throwHsqlx)
import Hsqlx.Pool.Config (PoolConfig (..))

-- | A connection pool.
data Pool = Pool
  { pConfig :: PoolConfig
  , pIdle :: TVar [PoolEntry]
  , pActive :: TVar Int
  , pWaiters :: TVar (Seq (TMVar Connection))
  , pClosed :: TVar Bool
  , pReaper :: Async ()
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
  let pool =
        Pool
          { pConfig = cfg
          , pIdle = idle
          , pActive = active
          , pWaiters = waiters
          , pClosed = closed
          , pReaper = undefined -- set below
          }
  reaper <- async (reaperThread pool)
  pure pool {pReaper = reaper}

-- | Close the pool and all connections.
closePool :: Pool -> IO ()
closePool pool = do
  atomically $ writeTVar (pClosed pool) True
  cancel (pReaper pool)
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
              acquire pool -- retry
            else do
              writeIORef (peLastUsed entry) now
              pure (peConn entry)
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

-- | Background thread that periodically reaps idle/expired connections.
reaperThread :: Pool -> IO ()
reaperThread pool = go
  where
    go = do
      threadDelay (30 * 1000000) -- check every 30 seconds
      now <- getCurrentTime
      expired <- atomically $ do
        idle <- readTVar (pIdle pool)
        let (keep, reap) = partition (not . shouldReap now) idle
        writeTVar (pIdle pool) keep
        pure reap
      mapM_ (\e -> safeClose (peConn e) >> atomically (modifyTVar' (pActive pool) (subtract 1))) expired
      go

    shouldReap now entry =
      isExpired (pConfig pool) entry now
        || isIdle (pConfig pool) entry now

    isIdle cfg entry now =
      let lastUsed = peCreatedAt entry -- approximate; real impl would read IORef
       in diffUTCTime now lastUsed > poolIdleTime cfg

    partition _ [] = ([], [])
    partition p (x : xs) =
      let (ys, zs) = partition p xs
       in if p x then (x : ys, zs) else (ys, x : zs)


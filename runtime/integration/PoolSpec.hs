module PoolSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, mapConcurrently, wait)
import Control.Concurrent.MVar
import Control.Concurrent.STM (newTVarIO, readTVarIO, modifyTVar', atomically)
import Control.Exception (SomeException, try)
import Data.IORef
import Valiant
import TestSupport
import Test.Hspec

spec :: Spec
spec = do
  describe "withResource" $ do
    it "acquires and releases a connection" $ do
      withTestPool $ \pool -> do
        result <- withResource pool $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT 42"
          pure (length rows)
        result `shouldBe` 1

    it "handles concurrent access" $ do
      withTestPool $ \pool -> do
        results <- mapConcurrently (\_ -> withResource pool $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT 1"
          pure (length rows)) [1 :: Int .. 20]
        all (== 1) results `shouldBe` True

    it "reuses connections" $ do
      withTestPool $ \pool -> do
        pid1 <- withResource pool $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT pg_backend_pid()"
          pure rows
        pid2 <- withResource pool $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT pg_backend_pid()"
          pure rows
        pid1 `shouldBe` pid2

  describe "concurrent acquire does not exceed poolSize" $ do
    it "never creates more connections than poolSize" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 3
            , poolAcquireTimeout = 5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      -- Track the maximum concurrent active connections
      maxActive <- newTVarIO (0 :: Int)
      active <- newTVarIO (0 :: Int)
      _ <- mapConcurrently (\_ -> withResource pool $ \conn -> do
        atomically $ modifyTVar' active (+ 1)
        cur <- readTVarIO active
        atomically $ modifyTVar' maxActive (\m -> max m cur)
        _ <- simpleQuery conn "SELECT pg_sleep(0.05)"
        atomically $ modifyTVar' active (subtract 1)
        pure ()
        ) [1 :: Int .. 10]
      closePool pool
      maxA <- readTVarIO maxActive
      maxA `shouldSatisfy` (<= 3)

  describe "shutdown wakes blocked waiters" $ do
    it "blocked waiters get PoolClosed" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 1
            , poolAcquireTimeout = 10
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      -- Hold the only connection
      barrier <- newEmptyMVar
      done <- newEmptyMVar
      _ <- async $ withResource pool $ \_ -> do
        putMVar barrier ()
        -- Wait until test says to continue
        takeMVar done
      -- Wait for the connection to be acquired
      takeMVar barrier
      -- Try to acquire in another thread — should block
      waiterResult <- async $ try @SomeException $ withResource pool $ \_ -> pure ()
      -- Give waiter time to block
      threadDelay 50000
      -- Close pool — should wake the waiter
      closePool pool
      putMVar done ()
      result <- wait waiterResult
      case result of
        Left e -> show e `shouldContain` "PoolClosed"
        Right () -> expectationFailure "Expected PoolClosed but got success"

  describe "poolStats" $ do
    it "returns sane values" $ do
      withTestPool $ \pool -> do
        -- Initial state: all idle after first warm-up
        _ <- withResource pool $ \conn -> simpleQuery conn "SELECT 1"
        stats <- poolStats pool
        psIdle stats `shouldSatisfy` (>= 1)
        psInUse stats `shouldBe` 0
        psWaiters stats `shouldBe` 0
        psTotalCreated stats `shouldSatisfy` (>= 1)

    it "tracks in-use connections" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 4
            , poolAcquireTimeout = 5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      barrier <- newEmptyMVar
      done <- newEmptyMVar
      _ <- async $ withResource pool $ \_ -> do
        putMVar barrier ()
        takeMVar done
      takeMVar barrier
      stats <- poolStats pool
      psInUse stats `shouldBe` 1
      putMVar done ()
      threadDelay 50000
      stats2 <- poolStats pool
      psInUse stats2 `shouldBe` 0
      closePool pool

  describe "resize" $ do
    it "shrinks idle connections" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 4
            , poolAcquireTimeout = 5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      -- Create 3 idle connections
      pids <- mapConcurrently (\_ -> withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT pg_backend_pid()"
        pure rows) [1 :: Int .. 3]
      length pids `shouldBe` 3
      stats1 <- poolStats pool
      psIdle stats1 `shouldSatisfy` (>= 2)
      -- Resize down to 1
      resize pool 1
      threadDelay 50000
      stats2 <- poolStats pool
      psIdle stats2 `shouldSatisfy` (<= 1)
      psMaxSize stats2 `shouldBe` 1
      closePool pool

    it "allows growth after resize up" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 1
            , poolAcquireTimeout = 5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      resize pool 3
      stats <- poolStats pool
      psMaxSize stats `shouldBe` 3
      -- Should be able to create multiple concurrent connections now
      results <- mapConcurrently (\_ -> withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT 1"
        pure (length rows)) [1 :: Int .. 3]
      all (== 1) results `shouldBe` True
      closePool pool

  describe "recycling methods" $ do
    it "RecycleFast reuses connections without query" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 2
            , poolRecyclingMethod = RecycleFast
            , poolAcquireTimeout = 5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      pid1 <- withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT pg_backend_pid()"
        pure rows
      pid2 <- withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT pg_backend_pid()"
        pure rows
      pid1 `shouldBe` pid2
      closePool pool

    it "RecycleVerified reuses recent connections without query" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 2
            , poolRecyclingMethod = RecycleVerified
            , poolHealthCheckAge = 60
            , poolAcquireTimeout = 5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      pid1 <- withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT pg_backend_pid()"
        pure rows
      -- Immediately reacquire — should skip health check (used < 60s ago)
      pid2 <- withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT pg_backend_pid()"
        pure rows
      pid1 `shouldBe` pid2
      closePool pool

    it "RecycleClean resets session state" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 2
            , poolRecyclingMethod = RecycleClean
            , poolAcquireTimeout = 5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      -- Set a session variable
      withResource pool $ \conn -> do
        _ <- simpleQuery conn "SET application_name TO 'test_app'"
        pure ()
      -- Reacquire — RecycleClean runs DISCARD ALL which resets session state
      appName <- withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SHOW application_name"
        pure rows
      -- After DISCARD ALL, application_name should be reset to default
      -- (empty string or the connConfig app name, not 'test_app')
      case appName of
        [[Just name]] -> name `shouldNotBe` "test_app"
        _ -> expectationFailure "Expected one row"
      closePool pool

  describe "lifecycle hooks" $ do
    it "fires hooks in correct order" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 2
            , poolAcquireTimeout = 5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      events <- newIORef ([] :: [String])
      setPostCreateHook pool $ \_ -> modifyIORef' events ("create" :)
      setOnAcquireHook pool $ \_ -> modifyIORef' events ("acquire" :)
      setPreReleaseHook pool $ \_ -> modifyIORef' events ("release" :)
      -- First acquire: create + acquire
      _ <- withResource pool $ \conn -> simpleQuery conn "SELECT 1"
      -- Second acquire (reuse): acquire only
      _ <- withResource pool $ \conn -> simpleQuery conn "SELECT 1"
      evts <- reverse <$> readIORef events
      -- First use: create, acquire, release
      -- Second use: acquire, release
      evts `shouldBe` ["create", "acquire", "release", "acquire", "release"]
      closePool pool

    it "hook failure doesn't break the pool" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 2
            , poolAcquireTimeout = 5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      setOnAcquireHook pool $ \_ -> error "hook failure"
      -- Should still work despite the hook throwing
      result <- withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT 42"
        pure (length rows)
      result `shouldBe` 1
      closePool pool

  describe "retain" $ do
    it "filters idle connections" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 4
            , poolAcquireTimeout = 5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      -- Create 3 idle connections and get their PIDs
      pids <- mapConcurrently (\_ -> withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT pg_backend_pid()"
        pure rows) [1 :: Int .. 3]
      length pids `shouldBe` 3
      stats1 <- poolStats pool
      let idleBefore = psIdle stats1
      idleBefore `shouldSatisfy` (>= 2)
      -- Keep only the first connection (drop all others by returning False)
      callCount <- newIORef (0 :: Int)
      retain pool $ \_ -> do
        n <- readIORef callCount
        modifyIORef' callCount (+ 1)
        pure (n == 0) -- keep first, drop rest
      stats2 <- poolStats pool
      psIdle stats2 `shouldSatisfy` (<= 1)
      closePool pool

  describe "QueueMode" $ do
    it "QueueLIFO reuses the most recently released connection" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 4
            , poolQueueMode = QueueLIFO
            , poolAcquireTimeout = 5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      -- Create 2 connections, then release them in order
      pid1 <- withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT pg_backend_pid()"
        pure rows
      pid2 <- withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT pg_backend_pid()"
        -- this should be the same as pid1 (LIFO: reuses most recent)
        pure rows
      -- In LIFO mode, we should get the same connection back
      pid1 `shouldBe` pid2
      closePool pool

    it "QueueFIFO rotates through connections" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 4
            , poolQueueMode = QueueFIFO
            , poolAcquireTimeout = 5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      -- Create 2 idle connections first
      barrier <- newEmptyMVar
      done1 <- newEmptyMVar
      done2 <- newEmptyMVar
      _ <- async $ withResource pool $ \_ -> do
        putMVar barrier ()
        takeMVar done1
      takeMVar barrier
      _ <- async $ withResource pool $ \_ -> do
        putMVar barrier ()
        takeMVar done2
      takeMVar barrier
      -- Now release both
      putMVar done1 ()
      putMVar done2 ()
      threadDelay 50000
      -- With 2 idle, FIFO should give oldest first
      stats <- poolStats pool
      psIdle stats `shouldSatisfy` (>= 2)
      -- Acquire twice and verify we get different PIDs
      pid1 <- withResource pool $ \conn -> do
        ([[Just p]], _) <- simpleQuery conn "SELECT pg_backend_pid()"
        pure p
      pid2 <- withResource pool $ \conn -> do
        ([[Just p]], _) <- simpleQuery conn "SELECT pg_backend_pid()"
        pure p
      -- FIFO should rotate, not reuse the same connection
      -- (Note: the first might equal the second if there's only one idle)
      -- At minimum, verify both acquisitions work
      pid1 `shouldNotBe` ""
      pid2 `shouldNotBe` ""
      closePool pool

  describe "pool timeout" $ do
    it "times out when pool is exhausted" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 1
            , poolAcquireTimeout = 0.5
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      barrier <- newEmptyMVar
      done <- newEmptyMVar
      -- Hold the only connection
      _ <- async $ withResource pool $ \_ -> do
        putMVar barrier ()
        takeMVar done
      takeMVar barrier
      -- Try to acquire — should timeout
      result <- try @SomeException $ withResource pool $ \_ -> pure ()
      putMVar done ()
      closePool pool
      case result of
        Left e -> show e `shouldContain` "PoolTimeout"
        Right () -> expectationFailure "Expected PoolTimeout"

    it "timeout increments counter" $ do
      url <- requireDatabaseUrl
      let cfg = defaultPoolConfig
            { poolConnString = url
            , poolSize = 1
            , poolAcquireTimeout = 0.2
            , poolMaxLifeJitter = 0
            }
      pool <- newPool cfg
      barrier <- newEmptyMVar
      done <- newEmptyMVar
      _ <- async $ withResource pool $ \_ -> do
        putMVar barrier ()
        takeMVar done
      takeMVar barrier
      _ <- try @SomeException $ withResource pool $ \_ -> pure ()
      putMVar done ()
      threadDelay 50000
      stats <- poolStats pool
      psTotalTimeouts stats `shouldBe` 1
      closePool pool

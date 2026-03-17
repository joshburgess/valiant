{-# LANGUAGE BangPatterns #-}

-- | Concurrent benchmarks that demonstrate automatic pipelining.
--
-- When N green threads submit queries on the same connection, the
-- writer thread coalesces their messages into a single sendMany
-- syscall and the reader demuxes responses by FIFO order.
-- This is where the async split architecture pays off.
module BenchConcurrent (benchmarks) where

import Control.Concurrent.Async (mapConcurrently)
import Criterion.Main
import Data.Int (Int32, Int64)
import Data.IORef
import Data.Text (Text)
import Hsqlx
import System.IO.Unsafe (unsafePerformIO)
import TestSupport (requireDatabaseUrl, insertBulkUsers)

{-# NOINLINE globalConn #-}
globalConn :: IORef (Maybe Connection)
globalConn = unsafePerformIO (newIORef Nothing)

{-# NOINLINE globalPool #-}
globalPool :: IORef (Maybe Pool)
globalPool = unsafePerformIO (newIORef Nothing)

getConn :: IO Connection
getConn = do
  mc <- readIORef globalConn
  case mc of
    Just c -> pure c
    Nothing -> do
      url <- requireDatabaseUrl
      c <- connectString url
      _ <- simpleQuery c "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, email TEXT, is_active BOOLEAN NOT NULL DEFAULT true, created_at TIMESTAMPTZ NOT NULL DEFAULT now())"
      (rows, _) <- simpleQuery c "SELECT count(*) FROM users"
      case rows of
        [[Just "0"]] -> insertBulkUsers c 1000
        _ -> pure ()
      writeIORef globalConn (Just c)
      pure c

getPool :: IO Pool
getPool = do
  mp <- readIORef globalPool
  case mp of
    Just p -> pure p
    Nothing -> do
      url <- requireDatabaseUrl
      p <- newPool defaultPoolConfig
        { poolConnString = url
        , poolSize = 8
        , poolAcquireTimeout = 10
        }
      writeIORef globalPool (Just p)
      pure p

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "concurrent"
    [ bgroup "single-conn"
      -- N threads sharing ONE connection — the async split coalesces
      -- their messages into batched sends automatically.
      [ bench "1 thread x 100 queries" $
          whnfIO (concurrentOnConn 1 100)
      , bench "4 threads x 100 queries" $
          whnfIO (concurrentOnConn 4 100)
      , bench "16 threads x 100 queries" $
          whnfIO (concurrentOnConn 16 100)
      , bench "32 threads x 100 queries" $
          whnfIO (concurrentOnConn 32 100)
      ]
    , bgroup "pool"
      -- N threads going through the pool — more realistic production usage.
      [ bench "1 thread x 100 queries" $
          whnfIO (concurrentOnPool 1 100)
      , bench "4 threads x 100 queries" $
          whnfIO (concurrentOnPool 4 100)
      , bench "16 threads x 100 queries" $
          whnfIO (concurrentOnPool 16 100)
      , bench "32 threads x 100 queries" $
          whnfIO (concurrentOnPool 32 100)
      ]
    ]
  ]

-- | N threads each run M queries on the SAME connection.
concurrentOnConn :: Int -> Int -> IO ()
concurrentOnConn nThreads nQueries = do
  conn <- getConn
  _ <- mapConcurrently (\_ -> runQueries conn nQueries) [1 .. nThreads]
  pure ()

-- | N threads each acquire from pool and run M queries.
concurrentOnPool :: Int -> Int -> IO ()
concurrentOnPool nThreads nQueries = do
  pool <- getPool
  _ <- mapConcurrently (\_ -> withResource pool $ \conn -> runQueries conn nQueries) [1 .. nThreads]
  pure ()

-- | Run a mix of queries on a connection.
runQueries :: Connection -> Int -> IO ()
runQueries conn n = go n
  where
    go 0 = pure ()
    go !i = do
      -- Alternate between a simple lookup and a count
      if even i
        then do
          _ <- fetchOne conn stmtSelectById (fromIntegral (i `mod` 1000 + 1) :: Int32)
          pure ()
        else do
          !_ <- fetchScalar conn stmtCount ()
          pure ()
      go (i - 1)

-- Statements ----------------------------------------------------------------

stmtSelectById :: Statement Int32 (Int32, Maybe Text)
stmtSelectById = mkStatement
  "SELECT id, name FROM users WHERE id = $1"
  [23] ["id", "name"] "<bench>"

stmtCount :: Statement () Int64
stmtCount = mkStatement "SELECT count(*) FROM users" [] ["count"] "<bench>"

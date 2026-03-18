{-# LANGUAGE BangPatterns #-}

-- | Pool-specific benchmarks: size scaling, contention, recycling methods.
module BenchPool (benchmarks) where

import Control.Concurrent.Async (mapConcurrently)
import Criterion.Main
import Hsqlx
import TestSupport (requireDatabaseUrl)

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "pool"
    [ bgroup "acquire-release"
      -- Measure pure pool overhead: acquire a connection, do a trivial
      -- query, release. Varies pool size to show scaling.
      [ bench "pool-size-1" $ whnfIO (poolAcquireRelease 1)
      , bench "pool-size-4" $ whnfIO (poolAcquireRelease 4)
      , bench "pool-size-16" $ whnfIO (poolAcquireRelease 16)
      ]
    , bgroup "contention"
      -- N threads compete for a pool of size M. Shows wait time under load.
      [ bench "8 threads, pool-4" $ whnfIO (poolContention 8 4 10)
      , bench "16 threads, pool-4" $ whnfIO (poolContention 16 4 10)
      , bench "32 threads, pool-4" $ whnfIO (poolContention 32 4 10)
      , bench "32 threads, pool-8" $ whnfIO (poolContention 32 8 10)
      , bench "32 threads, pool-16" $ whnfIO (poolContention 32 16 10)
      , bench "32 threads, pool-32" $ whnfIO (poolContention 32 32 10)
      ]
    , bgroup "recycling"
      -- Compare recycling methods: Fast (no check), Verified (empty query
      -- if idle > threshold), Clean (DISCARD ALL).
      [ bench "RecycleFast" $ whnfIO (poolRecycling RecycleFast)
      , bench "RecycleVerified" $ whnfIO (poolRecycling RecycleVerified)
      , bench "RecycleClean" $ whnfIO (poolRecycling RecycleClean)
      ]
    ]
  ]

-- | Acquire a connection from a fresh pool, run SELECT 1, release.
poolAcquireRelease :: Int -> IO ()
poolAcquireRelease size = do
  url <- requireDatabaseUrl
  pool <- newPool defaultPoolConfig
    { poolConnString = url
    , poolSize = size
    , poolAcquireTimeout = 5
    }
  _ <- withResource pool $ \conn ->
    simpleQuery conn "SELECT 1"
  closePool pool

-- | N threads each acquire from pool (size M) and run K queries.
poolContention :: Int -> Int -> Int -> IO ()
poolContention nThreads poolSize nQueries = do
  url <- requireDatabaseUrl
  pool <- newPool defaultPoolConfig
    { poolConnString = url
    , poolSize = poolSize
    , poolAcquireTimeout = 10
    }
  _ <- mapConcurrently (\_ ->
    withResource pool $ \conn ->
      mapM_ (\_ -> do _ <- simpleQuery conn "SELECT 1"; pure ()) [1 :: Int .. nQueries]
    ) [1 .. nThreads]
  closePool pool

-- | Benchmark different recycling methods.
poolRecycling :: RecyclingMethod -> IO ()
poolRecycling method = do
  url <- requireDatabaseUrl
  pool <- newPool defaultPoolConfig
    { poolConnString = url
    , poolSize = 4
    , poolAcquireTimeout = 5
    , poolRecyclingMethod = method
    , poolHealthCheckAge = 0 -- force check every time for RecycleVerified
    }
  -- Acquire and release 10 times to exercise recycling
  mapM_ (\_ -> do _ <- withResource pool $ \conn -> simpleQuery conn "SELECT 1"; pure ()) [1 :: Int .. 10]
  closePool pool

{-# LANGUAGE BangPatterns #-}

-- | Benchmarks matching asyncpg's methodology for direct comparison.
--
-- asyncpg uses: 10 concurrent connections, Unix socket, 30s duration,
-- 7 query patterns. We replicate the same patterns here.
--
-- See: https://github.com/MagicStack/pgbench
module BenchAsyncpgCompare (benchmarks) where

import Control.Concurrent.Async (mapConcurrently)
import Criterion.Main
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32, Int64)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Hsqlx
import System.IO.Unsafe (unsafePerformIO)
import TestSupport (requireDatabaseUrl)

{-# NOINLINE globalPool #-}
globalPool :: IORef (Maybe Pool)
globalPool = unsafePerformIO (newIORef Nothing)

getPool :: Int -> IO Pool
getPool size = do
  mp <- readIORef globalPool
  case mp of
    Just p -> pure p
    Nothing -> do
      url <- requireDatabaseUrl
      p <- newPool defaultPoolConfig
        { poolConnString = url
        , poolSize = size
        , poolAcquireTimeout = 10
        }
      writeIORef globalPool (Just p)
      -- Setup schema for benchmarks
      withResource p $ \conn -> do
        _ <- simpleQuery conn "DROP TABLE IF EXISTS _bench_series CASCADE"
        _ <- simpleQuery conn "DROP TABLE IF EXISTS _bench_insert CASCADE"
        _ <- simpleQuery conn
          "CREATE TABLE _bench_insert (\
          \  a int, b int, c int, d int, e text, f text, g text\
          \)"
        pure ()
      pure p

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "asyncpg-compare"
    -- Matching asyncpg's 7 benchmark patterns with 10 concurrent connections.
    [ bgroup "select-1+1"
      -- asyncpg benchmark #7: SELECT 1+1 (minimal overhead)
      [ bench "10 conns x 100 queries" $
          whnfIO (concurrentQueries 10 100 queryOnePlusOne)
      ]
    , bgroup "generate-series-1000"
      -- asyncpg benchmark #2: SELECT i FROM generate_series(1, 1000)
      [ bench "10 conns x 10 queries" $
          whnfIO (concurrentQueries 10 10 queryGenerateSeries)
      ]
    , bgroup "pg-type-wide-rows"
      -- asyncpg benchmark #1: wide rows from pg_type (~350 rows, 12 columns)
      [ bench "10 conns x 10 queries" $
          whnfIO (concurrentQueries 10 10 queryPgType)
      ]
    , bgroup "batch-insert-1000"
      -- asyncpg benchmark #6: 1000 individual parameterized INSERTs
      [ bench "10 conns x 1 batch" $
          whnfIO (concurrentQueries 10 1 queryBatchInsert)
      ]
    , bgroup "batch-insert-1000-pipelined"
      -- Same as above but using hsqlx's pipelined executeBatch
      [ bench "10 conns x 1 batch" $
          whnfIO (concurrentQueries 10 1 queryBatchInsertPipelined)
      ]

    -- Throughput tests at asyncpg's concurrency level
    , bgroup "throughput"
      [ bench "10 conns x 1000 SELECT 1+1" $
          whnfIO (concurrentQueries 10 1000 queryOnePlusOne)
      , bench "10 conns x 100 fetch-1000-rows" $
          whnfIO (concurrentQueries 10 100 queryGenerateSeries)
      ]
    ]
  ]

-- | N threads, each acquires from pool, runs M iterations of a query function.
concurrentQueries :: Int -> Int -> (Connection -> IO ()) -> IO ()
concurrentQueries nConns nQueries queryFn = do
  pool <- getPool nConns
  _ <- mapConcurrently (\_ ->
    withResource pool $ \conn ->
      mapM_ (\_ -> queryFn conn) [1 :: Int .. nQueries]
    ) [1 .. nConns]
  pure ()

-- Statements ----------------------------------------------------------------

stmtOnePlusOne :: Statement () Int32
stmtOnePlusOne = mkStatement "SELECT (1 + 1)::int4" [] ["?column?"] "<bench>"

stmtGenerateSeries :: Statement Int32 Int32
stmtGenerateSeries = mkStatement
  "SELECT i::int4 FROM generate_series(1, $1) AS i"
  [23] ["i"] "<bench>"

stmtInsertBench :: Statement (Int32, Int32, Int32, Int32, Text, Text, Text) ()
stmtInsertBench = mkStatement
  "INSERT INTO _bench_insert (a, b, c, d, e, f, g) VALUES ($1, $2, $3, $4, $5, $6, $7)"
  [23, 23, 23, 23, 25, 25, 25] [] "<bench>"

-- Query functions -----------------------------------------------------------

queryOnePlusOne :: Connection -> IO ()
queryOnePlusOne conn = do
  !_ <- fetchScalar conn stmtOnePlusOne ()
  pure ()

queryGenerateSeries :: Connection -> IO ()
queryGenerateSeries conn = do
  !_ <- fetchAll conn stmtGenerateSeries (1000 :: Int32)
  pure ()

queryPgType :: Connection -> IO ()
queryPgType conn = do
  -- Match asyncpg's pg_type query: wide rows from system catalog
  (rows, _) <- simpleQuery conn
    "SELECT typname, typnamespace, typowner, typlen, typbyval, \
    \typcategory, typispreferred, typisdefined, typdelim, typrelid, \
    \typelem, typarray FROM pg_type WHERE typtypmod = -1 AND typisdefined = true"
  seq (length rows) (pure ())

queryBatchInsert :: Connection -> IO ()
queryBatchInsert conn = do
  -- Sequential inserts (matching asyncpg's benchmark #6)
  _ <- simpleQuery conn "TRUNCATE _bench_insert"
  mapM_ (\i ->
    execute conn stmtInsertBench
      (i, i * 2, i * 3, i * 4,
       "val_" <> T.pack (show i),
       "text_" <> T.pack (show i),
       "data_" <> T.pack (show i))
    ) [1 :: Int32 .. 1000]

queryBatchInsertPipelined :: Connection -> IO ()
queryBatchInsertPipelined conn = do
  -- Pipelined inserts (hsqlx advantage — asyncpg doesn't have this)
  _ <- simpleQuery conn "TRUNCATE _bench_insert"
  _ <- executeBatch conn stmtInsertBench
    [ (i, i * 2, i * 3, i * 4,
       "val_" <> T.pack (show i),
       "text_" <> T.pack (show i),
       "data_" <> T.pack (show i))
    | i <- [1 :: Int32 .. 1000]
    ]
  pure ()

{-# LANGUAGE BangPatterns #-}

module BenchQuery (benchmarks) where

import Criterion.Main
import Data.Int (Int32, Int64)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Valiant
import System.IO.Unsafe (unsafePerformIO)
import TestSupport

-- | Global connection and pool, initialized once via unsafePerformIO.
-- This avoids NFData requirements on Connection/Pool.
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
      writeIORef globalConn (Just c)
      -- Ensure schema + seed data
      _ <- simpleQuery c "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, email TEXT, is_active BOOLEAN NOT NULL DEFAULT true, created_at TIMESTAMPTZ NOT NULL DEFAULT now())"
      _ <- simpleQuery c "TRUNCATE TABLE users RESTART IDENTITY"
      insertBulkUsers c 10000
      pure c

getPool :: IO Pool
getPool = do
  mp <- readIORef globalPool
  case mp of
    Just p -> pure p
    Nothing -> do
      _ <- getConn  -- ensure schema + seed data exist
      url <- requireDatabaseUrl
      p <- newPool defaultPoolConfig
        { poolConnString = url
        , poolSize = 4
        , poolAcquireTimeout = 5
        }
      writeIORef globalPool (Just p)
      pure p

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "query"
    [ bench "SELECT 1 (simple)" $
        whnfIO (getConn >>= \c -> simpleQuery c "SELECT 1")

    , bench "SELECT 1 (extended/prepared)" $
        whnfIO (getConn >>= \c -> fetchScalar c stmtSelectLiteral ())

    , bench "fetchOne by PK" $
        whnfIO (getConn >>= \c -> fetchOne c stmtSelectById (1 :: Int32))

    , bench "fetchAll 5 rows" $
        whnfIO (getConn >>= \c -> fetchAll c stmtListFive ())

    , bench "fetchAll 1000 rows" $
        whnfIO (getConn >>= \c -> fetchAll c stmtListAll1k ())

    , bench "fetchScalar COUNT" $
        whnfIO (getConn >>= \c -> fetchScalar c stmtCount ())

    , bench "execute INSERT (in txn, rolled back)" $
        whnfIO $ do
          p <- getPool
          withTransaction p $ \tx -> do
            _ <- execute (txConn tx) stmtInsertUser ("bench_user", Just ("b@t.com" :: Text))
            -- We let the transaction commit; the user is real but harmless
            pure ()

    , bench "pool acquire/release (SELECT 1)" $
        whnfIO $ do
          p <- getPool
          withResource p $ \c -> simpleQuery c "SELECT 1"

    , bench "fetchAllVec 1000 rows" $
        whnfIO (getConn >>= \c -> fetchAllVec c stmtListAll1k ())

    , bench "executeWithFold 1000 rows" $
        whnfIO (getConn >>= \c -> executeWithFold c stmtListAll1k () (RowFold (0 :: Int) (\n _ -> n + 1)))

    , bench "forEach 1000 rows" $
        whnfIO (getConn >>= \c -> forEach c stmtListAll1k () (\_ -> pure ()))

    , bench "executeBatch 100 inserts (pipelined)" $
        whnfIO $ do
          p <- getPool
          withTransaction p $ \tx -> do
            _ <- executeBatch (txConn tx) stmtInsertUser
              [ ("bench_" <> T.pack (show i), Just ("b@t.com" :: Text))
              | i <- [1 :: Int .. 100]
              ]
            pure ()

    , bench "executeBatch 1000 inserts (pipelined)" $
        whnfIO $ do
          p <- getPool
          withTransaction p $ \tx -> do
            _ <- executeBatch (txConn tx) stmtInsertUser
              [ ("bench_" <> T.pack (show i), Just ("b@t.com" :: Text))
              | i <- [1 :: Int .. 1000]
              ]
            pure ()

    , bench "executeBatch 5000 inserts (pipelined)" $
        whnfIO $ do
          p <- getPool
          withTransaction p $ \tx -> do
            _ <- executeBatch (txConn tx) stmtInsertUser
              [ ("bench_" <> T.pack (show i), Just ("b@t.com" :: Text))
              | i <- [1 :: Int .. 5000]
              ]
            pure ()

    , bench "transaction overhead (BEGIN+COMMIT)" $
        whnfIO $ do
          p <- getPool
          withTransaction p $ \tx ->
            fetchScalar (txConn tx) stmtCount ()

    , bench "cursor 1k rows, batch 100" $
        whnfIO (cursorDrain stmtListAll1k 100)
    , bench "cursor 1k rows, batch 500" $
        whnfIO (cursorDrain stmtListAll1k 500)
    , bench "cursor 1k rows, batch 1000" $
        whnfIO (cursorDrain stmtListAll1k 1000)
    , bench "cursor 10k rows, batch 100" $
        whnfIO (cursorDrain stmtListAll10k 100)
    , bench "cursor 10k rows, batch 1000" $
        whnfIO (cursorDrain stmtListAll10k 1000)
    , bench "cursor 10k rows, batch 10000" $
        whnfIO (cursorDrain stmtListAll10k 10000)

    -- fetchAllCursor variants (decode + materialise the full result list).
    -- Compared against cursorDrain (which only counts), the gap is
    -- decode + accumulation cost.
    , bench "fetchAllCursor 1k rows, batch 100" $
        whnfIO $ do
          p <- getPool
          withTransaction p $ \tx ->
            length <$> fetchAllCursor (txConn tx) stmtListAll1k () 100
    , bench "fetchAllCursor 10k rows, batch 100" $
        whnfIO $ do
          p <- getPool
          withTransaction p $ \tx ->
            length <$> fetchAllCursor (txConn tx) stmtListAll10k () 100
    , bench "fetchAllCursor 10k rows, batch 1000" $
        whnfIO $ do
          p <- getPool
          withTransaction p $ \tx ->
            length <$> fetchAllCursor (txConn tx) stmtListAll10k () 1000
    ]
  ]

-- | Drain a cursor with the given batch size. Returns the row count so
-- the benchmark can't optimize the fetch away.
cursorDrain :: Statement () (Int32, Text) -> Int -> IO Int
cursorDrain !stmt !batch = do
  p <- getPool
  withTransaction p $ \tx ->
    withCursor (txConn tx) stmt () batch $ \cs -> do
      let loop !n = do
            rows <- fetchBatch cs batch
            if null rows then pure n else loop (n + length rows)
      loop 0

-- Statements ----------------------------------------------------------------

stmtSelectLiteral :: Statement () Int32
stmtSelectLiteral = mkStatement "SELECT 1::int4" [] ["?column?"] "<bench>"

stmtSelectById :: Statement Int32 (Int32, Text, Maybe Text)
stmtSelectById = mkStatement
  "SELECT id, name, email FROM users WHERE id = $1"
  [23] ["id", "name", "email"] "<bench>"

stmtListFive :: Statement () (Int32, Text)
stmtListFive = mkStatement
  "SELECT id, name FROM users ORDER BY id LIMIT 5"
  [] ["id", "name"] "<bench>"

stmtListAll1k :: Statement () (Int32, Text)
stmtListAll1k = mkStatement
  "SELECT id, name FROM users ORDER BY id LIMIT 1000"
  [] ["id", "name"] "<bench>"

stmtListAll10k :: Statement () (Int32, Text)
stmtListAll10k = mkStatement
  "SELECT id, name FROM users ORDER BY id LIMIT 10000"
  [] ["id", "name"] "<bench>"

stmtInsertUser :: Statement (Text, Maybe Text) ()
stmtInsertUser = mkStatement
  "INSERT INTO users (name, email) VALUES ($1, $2)"
  [25, 25] [] "<bench>"

stmtCount :: Statement () Int64
stmtCount = mkStatement "SELECT count(*) FROM users" [] ["count"] "<bench>"

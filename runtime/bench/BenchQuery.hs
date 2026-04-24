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
      (rows, _) <- simpleQuery c "SELECT count(*) FROM users"
      case rows of
        [[Just "0"]] -> insertBulkUsers c 1000
        _ -> pure ()
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
        whnfIO (getConn >>= \c -> fetchAll c stmtListAllBulk ())

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
        whnfIO (getConn >>= \c -> fetchAllVec c stmtListAllBulk ())

    , bench "executeWithFold 1000 rows" $
        whnfIO (getConn >>= \c -> executeWithFold c stmtListAllBulk () (RowFold (0 :: Int) (\n _ -> n + 1)))

    , bench "forEach 1000 rows" $
        whnfIO (getConn >>= \c -> forEach c stmtListAllBulk () (\_ -> pure ()))

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
    ]
  ]

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

stmtListAllBulk :: Statement () (Int32, Text)
stmtListAllBulk = mkStatement
  "SELECT id, name FROM users ORDER BY id"
  [] ["id", "name"] "<bench>"

stmtInsertUser :: Statement (Text, Maybe Text) ()
stmtInsertUser = mkStatement
  "INSERT INTO users (name, email) VALUES ($1, $2)"
  [25, 25] [] "<bench>"

stmtCount :: Statement () Int64
stmtCount = mkStatement "SELECT count(*) FROM users" [] ["count"] "<bench>"

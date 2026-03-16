module BenchHsqlx
  ( setup
  , selectOne
  , fetchOneByPK
  , fetchN
  , insertN
  , updateN
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Hsqlx
import System.IO.Unsafe (unsafePerformIO)

-- Cached connection
{-# NOINLINE connRef #-}
connRef :: IORef (Maybe Connection)
connRef = unsafePerformIO (newIORef Nothing)

getConn :: ByteString -> IO Connection
getConn url = do
  mc <- readIORef connRef
  case mc of
    Just c -> pure c
    Nothing -> do
      c <- connectString url
      writeIORef connRef (Just c)
      pure c

-- Schema setup (called once from Main)
setup :: ByteString -> Int -> IO ()
setup url n = do
  conn <- getConn url
  _ <- simpleQuery conn "DROP TABLE IF EXISTS bench_users CASCADE"
  _ <- simpleQuery conn
    "CREATE TABLE bench_users (\
    \  id SERIAL PRIMARY KEY,\
    \  name TEXT NOT NULL,\
    \  email TEXT,\
    \  score INTEGER NOT NULL DEFAULT 0\
    \)"
  _ <- simpleQuery conn "CREATE INDEX ON bench_users (score)"
  -- Seed rows in batches of 1000 to avoid query size limits
  let batchSize = 1000
      batches = (n + batchSize - 1) `div` batchSize
  mapM_ (\batch -> do
    let start = (batch - 1) * batchSize + 1
        end = min (batch * batchSize) n
        values = BS8.intercalate ","
          [ "('user_" <> BS8.pack (show i) <> "', 'user" <> BS8.pack (show i) <> "@test.com', " <> BS8.pack (show (i `mod` 100)) <> ")"
          | i <- [start .. end]
          ]
    _ <- simpleQuery conn ("INSERT INTO bench_users (name, email, score) VALUES " <> values)
    pure ()
    ) [1 .. batches]
  -- Analyze for accurate query plans
  _ <- simpleQuery conn "ANALYZE bench_users"
  pure ()

-- Statements
stmtSelect1 :: Statement () Int32
stmtSelect1 = mkStatement "SELECT 1::int4" [] ["?column?"] "<bench>"

stmtFetchOne :: Statement Int32 (Int32, Text, Maybe Text, Int32)
stmtFetchOne = mkStatement
  "SELECT id, name, email, score FROM bench_users WHERE id = $1"
  [23] ["id", "name", "email", "score"] "<bench>"

stmtFetchN :: Statement Int32 (Int32, Text, Maybe Text, Int32)
stmtFetchN = mkStatement
  "SELECT id, name, email, score FROM bench_users ORDER BY id LIMIT $1"
  [23] ["id", "name", "email", "score"] "<bench>"

stmtInsert :: Statement (Text, Maybe Text) ()
stmtInsert = mkStatement
  "INSERT INTO bench_users (name, email) VALUES ($1, $2)"
  [25, 25] [] "<bench>"

stmtUpdateByScore :: Statement (Int32, Int32) ()
stmtUpdateByScore = mkStatement
  "UPDATE bench_users SET score = score + $1 WHERE score < $2"
  [23, 23] [] "<bench>"

-- Benchmarks
selectOne :: ByteString -> IO ()
selectOne url = do
  conn <- getConn url
  _ <- fetchScalar conn stmtSelect1 ()
  pure ()

fetchOneByPK :: ByteString -> IO ()
fetchOneByPK url = do
  conn <- getConn url
  _ <- fetchOne conn stmtFetchOne (1 :: Int32)
  pure ()

fetchN :: ByteString -> Int -> IO ()
fetchN url n = do
  conn <- getConn url
  _ <- fetchAll conn stmtFetchN (fromIntegral n :: Int32)
  pure ()

insertN :: ByteString -> Int -> IO ()
insertN url n = do
  conn <- getConn url
  mapM_ (\i -> do
    let name = "ins_" <> T.pack (show i)
        email = Just (name <> "@test.com")
    execute conn stmtInsert (name, email)
    ) [1 :: Int .. n]

updateN :: ByteString -> Int -> IO ()
updateN url n = do
  conn <- getConn url
  -- Reset scores first so the update always has work to do
  _ <- simpleQuery conn "UPDATE bench_users SET score = id % 100"
  -- Update rows where score < threshold, affecting ~n rows
  -- score is i % 100, so score < t affects t% of 10000 rows
  let threshold = max 1 (min 100 (fromIntegral n * 100 `div` 10000)) :: Int32
  _ <- execute conn stmtUpdateByScore (1, threshold)
  pure ()

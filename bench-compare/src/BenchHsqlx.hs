module BenchHsqlx
  ( setup
  , selectOne
  , fetchOneByPK
  , fetchAll1000
  , insertOne
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Int (Int32)
import Data.Text (Text)
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
setup :: ByteString -> IO ()
setup url = do
  conn <- getConn url
  _ <- simpleQuery conn "DROP TABLE IF EXISTS bench_users CASCADE"
  _ <- simpleQuery conn
    "CREATE TABLE bench_users (\
    \  id SERIAL PRIMARY KEY,\
    \  name TEXT NOT NULL,\
    \  email TEXT\
    \)"
  -- Seed 1000 rows
  let values = BS8.intercalate ","
        [ "('user_" <> BS8.pack (show i) <> "', 'user" <> BS8.pack (show i) <> "@test.com')"
        | i <- [1 :: Int .. 1000]
        ]
  _ <- simpleQuery conn ("INSERT INTO bench_users (name, email) VALUES " <> values)
  pure ()

-- Statements
stmtSelect1 :: Statement () Int32
stmtSelect1 = mkStatement "SELECT 1::int4" [] ["?column?"] "<bench>"

stmtFetchOne :: Statement Int32 (Int32, Text, Maybe Text)
stmtFetchOne = mkStatement
  "SELECT id, name, email FROM bench_users WHERE id = $1"
  [23] ["id", "name", "email"] "<bench>"

stmtFetchAll :: Statement () (Int32, Text)
stmtFetchAll = mkStatement
  "SELECT id, name FROM bench_users ORDER BY id"
  [] ["id", "name"] "<bench>"

stmtInsert :: Statement (Text, Maybe Text) ()
stmtInsert = mkStatement
  "INSERT INTO bench_users (name, email) VALUES ($1, $2)"
  [25, 25] [] "<bench>"

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

fetchAll1000 :: ByteString -> IO ()
fetchAll1000 url = do
  conn <- getConn url
  _ <- fetchAll conn stmtFetchAll ()
  pure ()

insertOne :: ByteString -> IO ()
insertOne url = do
  conn <- getConn url
  _ <- execute conn stmtInsert ("bench_insert", Just ("bench@test.com" :: Text))
  pure ()

module BenchPgSimple
  ( selectOne
  , fetchOneByPK
  , fetchAll1000
  , insertOne
  ) where

import Data.ByteString (ByteString)
import Data.IORef
import Data.Int (Int32)
import Data.Text (Text)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple (Only (..))
import System.IO.Unsafe (unsafePerformIO)

-- Cached connection
{-# NOINLINE connRef #-}
connRef :: IORef (Maybe PG.Connection)
connRef = unsafePerformIO (newIORef Nothing)

getConn :: ByteString -> IO PG.Connection
getConn url = do
  mc <- readIORef connRef
  case mc of
    Just c -> pure c
    Nothing -> do
      c <- PG.connectPostgreSQL url
      writeIORef connRef (Just c)
      pure c

-- Benchmarks
selectOne :: ByteString -> IO ()
selectOne url = do
  conn <- getConn url
  [Only (_ :: Int32)] <- PG.query_ conn "SELECT 1::int4"
  pure ()

fetchOneByPK :: ByteString -> IO ()
fetchOneByPK url = do
  conn <- getConn url
  _ <- PG.query conn
    "SELECT id, name, email FROM bench_users WHERE id = ?"
    (Only (1 :: Int32)) :: IO [(Int32, Text, Maybe Text)]
  pure ()

fetchAll1000 :: ByteString -> IO ()
fetchAll1000 url = do
  conn <- getConn url
  _ <- PG.query_ conn
    "SELECT id, name FROM bench_users ORDER BY id" :: IO [(Int32, Text)]
  pure ()

insertOne :: ByteString -> IO ()
insertOne url = do
  conn <- getConn url
  _ <- PG.execute conn
    "INSERT INTO bench_users (name, email) VALUES (?, ?)"
    ("bench_pgsimple" :: Text, Just ("bench@test.com" :: Text))
  pure ()

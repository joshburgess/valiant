module BenchPgSimple
  ( selectOne
  , fetchOneByPK
  , fetchN
  , insertN
  , updateN
  ) where

import Data.ByteString (ByteString)
import Data.IORef
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
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
    "SELECT id, name, email, score FROM bench_users WHERE id = ?"
    (Only (1 :: Int32)) :: IO [(Int32, Text, Maybe Text, Int32)]
  pure ()

fetchN :: ByteString -> Int -> IO ()
fetchN url n = do
  conn <- getConn url
  _ <- PG.query conn
    "SELECT id, name, email, score FROM bench_users ORDER BY id LIMIT ?"
    (Only (fromIntegral n :: Int32)) :: IO [(Int32, Text, Maybe Text, Int32)]
  pure ()

insertN :: ByteString -> Int -> IO ()
insertN url n = do
  conn <- getConn url
  mapM_ (\i -> do
    let name = "ins_" <> T.pack (show i) :: Text
        email = Just (name <> "@test.com") :: Maybe Text
    PG.execute conn
      "INSERT INTO bench_users (name, email) VALUES (?, ?)"
      (name, email)
    ) [1 :: Int .. n]

updateN :: ByteString -> Int -> IO ()
updateN url n = do
  conn <- getConn url
  _ <- PG.execute conn
    "UPDATE bench_users SET score = score + ? WHERE id <= ?"
    (1 :: Int32, fromIntegral n :: Int32)
  pure ()

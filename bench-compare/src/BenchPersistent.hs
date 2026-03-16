module BenchPersistent
  ( selectOne
  , fetchOneByPK
  , fetchN
  , insertN
  , updateN
  ) where

import Control.Monad (forM_)
import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Persist.Sql (Single (..), rawSql, rawExecute, runSqlPool, SqlBackend)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Types (PersistValue (..))
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE poolRef #-}
poolRef :: IORef (Maybe (Pool SqlBackend))
poolRef = unsafePerformIO (newIORef Nothing)

getPool :: ByteString -> IO (Pool SqlBackend)
getPool url = do
  mp <- readIORef poolRef
  case mp of
    Just p -> pure p
    Nothing -> do
      p <- runNoLoggingT $ createPostgresqlPool url 1
      writeIORef poolRef (Just p)
      pure p

selectOne :: ByteString -> IO ()
selectOne url = do
  pool <- getPool url
  _ <- runSqlPool (rawSql "SELECT 1::int4" []) pool :: IO [Single Int]
  pure ()

fetchOneByPK :: ByteString -> IO ()
fetchOneByPK url = do
  pool <- getPool url
  _ <- runSqlPool
    (rawSql "SELECT id, name, email, score FROM bench_users WHERE id = ?"
      [PersistInt64 1])
    pool :: IO [(Single Int, Single Text, Single (Maybe Text), Single Int)]
  pure ()

fetchN :: ByteString -> Int -> IO ()
fetchN url n = do
  pool <- getPool url
  _ <- runSqlPool
    (rawSql "SELECT id, name, email, score FROM bench_users ORDER BY id LIMIT ?"
      [PersistInt64 (fromIntegral n)])
    pool :: IO [(Single Int, Single Text, Single (Maybe Text), Single Int)]
  pure ()

insertN :: ByteString -> Int -> IO ()
insertN url n = do
  pool <- getPool url
  runSqlPool (forM_ [1 :: Int .. n] $ \i -> do
    let name = "ins_" <> T.pack (show i)
        email = "ins_" <> T.pack (show i) <> "@test.com"
    rawExecute "INSERT INTO bench_users (name, email) VALUES (?, ?)"
      [PersistText name, PersistText email]
    ) pool

updateN :: ByteString -> Int -> IO ()
updateN url n = do
  pool <- getPool url
  runSqlPool
    (rawExecute "UPDATE bench_users SET score = score + ? WHERE id <= ?"
      [PersistInt64 1, PersistInt64 (fromIntegral n)])
    pool

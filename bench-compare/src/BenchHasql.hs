module BenchHasql
  ( selectOne
  , fetchOneByPK
  , fetchAll1000
  , insertOne
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32)
import Data.IORef
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Functor.Contravariant ((>$<))
import Hasql.Connection qualified as HC
import Hasql.Session qualified as HS
import Hasql.Statement qualified as HSt
import Hasql.Decoders qualified as HD
import Hasql.Encoders qualified as HE
import System.IO.Unsafe (unsafePerformIO)

-- Cached connection
{-# NOINLINE connRef #-}
connRef :: IORef (Maybe HC.Connection)
connRef = unsafePerformIO (newIORef Nothing)

getConn :: String -> IO HC.Connection
getConn url = do
  mc <- readIORef connRef
  case mc of
    Just c -> pure c
    Nothing -> do
      result <- HC.acquire (HC.settings (BS8.pack host) (fromIntegral port) (BS8.pack user) (BS8.pack pass) (BS8.pack db))
      case result of
        Left err -> error $ "hasql connection failed: " <> show err
        Right c -> do
          writeIORef connRef (Just c)
          pure c
  where
    -- Parse from URL: postgres://user:pass@host:port/db
    -- Simple extraction for benchmark purposes
    (user, pass, host, port, db) = parseUrl url

parseUrl :: String -> (String, String, String, Int, String)
parseUrl url =
  let afterScheme = drop 2 $ dropWhile (/= '/') url -- drop "scheme://"
      (authHost, pathRest) = break (== '/') afterScheme
      dbName = drop 1 pathRest
      (auth, hostPort) = case break (== '@') authHost of
        (a, hp) | null hp -> ("", a)
                 | otherwise -> (a, drop 1 hp)
      (userName, password) = case break (== ':') auth of
        (u, p) | null p -> (u, "")
                | otherwise -> (u, drop 1 p)
      (hostName, portStr) = case break (== ':') hostPort of
        (h, p) | null p -> (h, "5432")
                | otherwise -> (h, drop 1 p)
      portNum = read portStr :: Int
  in (userName, password, hostName, portNum, dbName)

-- Statements
stmtSelect1 :: HSt.Statement () Int32
stmtSelect1 = HSt.Statement
  "SELECT 1::int4"
  HE.noParams
  (HD.singleRow (HD.column (HD.nonNullable HD.int4)))
  True

stmtFetchOne :: HSt.Statement Int32 (Maybe (Int32, Text, Maybe Text))
stmtFetchOne = HSt.Statement
  "SELECT id, name, email FROM bench_users WHERE id = $1"
  (HE.param (HE.nonNullable HE.int4))
  (HD.rowMaybe row)
  True
  where
    row = (,,)
      <$> HD.column (HD.nonNullable HD.int4)
      <*> HD.column (HD.nonNullable HD.text)
      <*> HD.column (HD.nullable HD.text)

stmtFetchAll :: HSt.Statement () (Vector (Int32, Text))
stmtFetchAll = HSt.Statement
  "SELECT id, name FROM bench_users ORDER BY id"
  HE.noParams
  (HD.rowVector row)
  True
  where
    row = (,)
      <$> HD.column (HD.nonNullable HD.int4)
      <*> HD.column (HD.nonNullable HD.text)

stmtInsert :: HSt.Statement (Text, Maybe Text) ()
stmtInsert = HSt.Statement
  "INSERT INTO bench_users (name, email) VALUES ($1, $2)"
  encoder
  HD.noResult
  True
  where
    encoder =
      (fst >$< HE.param (HE.nonNullable HE.text))
        <> (snd >$< HE.param (HE.nullable HE.text))

-- Benchmarks
selectOne :: String -> IO ()
selectOne url = do
  conn <- getConn url
  result <- HS.run (HS.statement () stmtSelect1) conn
  case result of
    Left err -> error $ "hasql selectOne: " <> show err
    Right _ -> pure ()

fetchOneByPK :: String -> IO ()
fetchOneByPK url = do
  conn <- getConn url
  result <- HS.run (HS.statement 1 stmtFetchOne) conn
  case result of
    Left err -> error $ "hasql fetchOneByPK: " <> show err
    Right _ -> pure ()

fetchAll1000 :: String -> IO ()
fetchAll1000 url = do
  conn <- getConn url
  result <- HS.run (HS.statement () stmtFetchAll) conn
  case result of
    Left err -> error $ "hasql fetchAll1000: " <> show err
    Right _ -> pure ()

insertOne :: String -> IO ()
insertOne url = do
  conn <- getConn url
  result <- HS.run (HS.statement ("bench_hasql", Just "bench@test.com") stmtInsert) conn
  case result of
    Left err -> error $ "hasql insertOne: " <> show err
    Right _ -> pure ()

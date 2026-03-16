module BenchHasql
  ( selectOne
  , fetchOneByPK
  , fetchN
  , insertN
  , updateN
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32, Int64)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
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
    (user, pass, host, port, db) = parseUrl url

parseUrl :: String -> (String, String, String, Int, String)
parseUrl url =
  let afterScheme = drop 2 $ dropWhile (/= '/') url
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

stmtFetchOne :: HSt.Statement Int32 (Maybe (Int32, Text, Maybe Text, Int32))
stmtFetchOne = HSt.Statement
  "SELECT id, name, email, score FROM bench_users WHERE id = $1"
  (HE.param (HE.nonNullable HE.int4))
  (HD.rowMaybe row)
  True
  where
    row = (,,,)
      <$> HD.column (HD.nonNullable HD.int4)
      <*> HD.column (HD.nonNullable HD.text)
      <*> HD.column (HD.nullable HD.text)
      <*> HD.column (HD.nonNullable HD.int4)

stmtFetchN :: HSt.Statement Int32 (Vector (Int32, Text, Maybe Text, Int32))
stmtFetchN = HSt.Statement
  "SELECT id, name, email, score FROM bench_users ORDER BY id LIMIT $1"
  (HE.param (HE.nonNullable HE.int4))
  (HD.rowVector row)
  True
  where
    row = (,,,)
      <$> HD.column (HD.nonNullable HD.int4)
      <*> HD.column (HD.nonNullable HD.text)
      <*> HD.column (HD.nullable HD.text)
      <*> HD.column (HD.nonNullable HD.int4)

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

stmtUpdate :: HSt.Statement (Int32, Int32) Int64
stmtUpdate = HSt.Statement
  "UPDATE bench_users SET score = score + $1 WHERE id <= $2"
  encoder
  HD.rowsAffected
  True
  where
    encoder =
      (fst >$< HE.param (HE.nonNullable HE.int4))
        <> (snd >$< HE.param (HE.nonNullable HE.int4))

-- Benchmarks
selectOne :: String -> IO ()
selectOne url = do
  conn <- getConn url
  result <- HS.run (HS.statement () stmtSelect1) conn
  case result of
    Left err -> error $ "hasql: " <> show err
    Right _ -> pure ()

fetchOneByPK :: String -> IO ()
fetchOneByPK url = do
  conn <- getConn url
  result <- HS.run (HS.statement 1 stmtFetchOne) conn
  case result of
    Left err -> error $ "hasql: " <> show err
    Right _ -> pure ()

fetchN :: String -> Int -> IO ()
fetchN url n = do
  conn <- getConn url
  result <- HS.run (HS.statement (fromIntegral n) stmtFetchN) conn
  case result of
    Left err -> error $ "hasql: " <> show err
    Right _ -> pure ()

insertN :: String -> Int -> IO ()
insertN url n = do
  conn <- getConn url
  mapM_ (\i -> do
    let name = "ins_" <> T.pack (show i)
        email = Just (name <> "@test.com")
    result <- HS.run (HS.statement (name, email) stmtInsert) conn
    case result of
      Left err -> error $ "hasql: " <> show err
      Right _ -> pure ()
    ) [1 :: Int .. n]

updateN :: String -> Int -> IO ()
updateN url n = do
  conn <- getConn url
  result <- HS.run (HS.statement (1, fromIntegral n :: Int32) stmtUpdate) conn
  case result of
    Left err -> error $ "hasql: " <> show err
    Right _ -> pure ()

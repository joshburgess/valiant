module TestSupport
  ( withTestConnection
  , withSchema
  , insertTestUsers
  , stmtListAll
  , stmtSelectOne
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32)
import Data.Text (Text)
import Valiant
import System.Environment (lookupEnv)

requireDatabaseUrl :: IO ByteString
requireDatabaseUrl = do
  mUrl <- lookupEnv "DATABASE_URL"
  case mUrl of
    Just url -> pure (BS8.pack url)
    Nothing -> error "DATABASE_URL is not set."

withTestConnection :: (Connection -> IO a) -> IO a
withTestConnection action = do
  url <- requireDatabaseUrl
  conn <- connectString url
  result <- action conn
  close conn
  pure result

withSchema :: Connection -> IO a -> IO a
withSchema conn action = do
  _ <- simpleQuery conn "DROP TABLE IF EXISTS users_bluefin CASCADE"
  _ <- simpleQuery conn
    "CREATE TABLE users_bluefin (\
    \  id SERIAL PRIMARY KEY,\
    \  name TEXT NOT NULL,\
    \  email TEXT\
    \)"
  result <- action
  _ <- simpleQuery conn "DROP TABLE IF EXISTS users_bluefin CASCADE"
  pure result

insertTestUsers :: Connection -> IO ()
insertTestUsers conn = do
  _ <- simpleQuery conn
    "INSERT INTO users_bluefin (name, email) VALUES \
    \('Alice', 'alice@example.com'),\
    \('Bob', 'bob@example.com'),\
    \('Carol', NULL),\
    \('Dave', 'dave@example.com'),\
    \('Eve', 'eve@example.com')"
  pure ()

stmtListAll :: Statement () (Int32, Text)
stmtListAll = mkStatement
  "SELECT id, name FROM users_bluefin ORDER BY id"
  [] ["id", "name"] "<test>"

stmtSelectOne :: Statement Int32 (Int32, Text, Maybe Text)
stmtSelectOne = mkStatement
  "SELECT id, name, email FROM users_bluefin WHERE id = $1"
  [23] ["id", "name", "email"] "<test>"

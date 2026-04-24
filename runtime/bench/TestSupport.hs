module TestSupport
  ( withTestConnection
  , withTestPool
  , requireDatabaseUrl
  , createSchema
  , dropSchema
  , withSchema
  , insertTestUsers
  , insertBulkUsers
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Valiant
import System.Environment (lookupEnv)

-- Connection helpers --------------------------------------------------------

requireDatabaseUrl :: IO ByteString
requireDatabaseUrl = do
  mUrl <- lookupEnv "DATABASE_URL"
  case mUrl of
    Just url -> pure (BS8.pack url)
    Nothing -> error
      "DATABASE_URL is not set.\n\n\
      \  Run: eval $(scripts/pg-setup.sh)\n\
      \  Or:  export DATABASE_URL=postgres://user:pass@localhost:5432/mydb\n"

withTestConnection :: (Connection -> IO a) -> IO a
withTestConnection action = do
  url <- requireDatabaseUrl
  conn <- connectString url
  result <- action conn
  close conn
  pure result

withTestPool :: (Pool -> IO a) -> IO a
withTestPool action = do
  url <- requireDatabaseUrl
  pool <- newPool defaultPoolConfig
    { poolConnString = url
    , poolSize = 4
    , poolAcquireTimeout = 5
    }
  result <- action pool
  closePool pool
  pure result

-- Schema --------------------------------------------------------------------

createSchema :: Connection -> IO ()
createSchema conn = do
  _ <- simpleQuery conn
    "CREATE TABLE IF NOT EXISTS users (\
    \  id SERIAL PRIMARY KEY,\
    \  name TEXT NOT NULL,\
    \  email TEXT,\
    \  is_active BOOLEAN NOT NULL DEFAULT true,\
    \  created_at TIMESTAMPTZ NOT NULL DEFAULT now()\
    \)"
  _ <- simpleQuery conn
    "CREATE TABLE IF NOT EXISTS posts (\
    \  id SERIAL PRIMARY KEY,\
    \  author_id INTEGER NOT NULL REFERENCES users(id),\
    \  title TEXT NOT NULL,\
    \  body TEXT,\
    \  published_at TIMESTAMPTZ,\
    \  created_at TIMESTAMPTZ NOT NULL DEFAULT now()\
    \)"
  pure ()

dropSchema :: Connection -> IO ()
dropSchema conn = do
  _ <- simpleQuery conn "DROP TABLE IF EXISTS posts CASCADE"
  _ <- simpleQuery conn "DROP TABLE IF EXISTS users CASCADE"
  pure ()

withSchema :: Connection -> IO a -> IO a
withSchema conn action = do
  dropSchema conn
  createSchema conn
  result <- action
  dropSchema conn
  pure result

-- Fixtures ------------------------------------------------------------------

insertTestUsers :: Connection -> IO ()
insertTestUsers conn = do
  _ <- simpleQuery conn
    "INSERT INTO users (name, email) VALUES \
    \('Alice', 'alice@example.com'),\
    \('Bob', 'bob@example.com'),\
    \('Carol', NULL),\
    \('Dave', 'dave@example.com'),\
    \('Eve', 'eve@example.com')"
  pure ()

insertBulkUsers :: Connection -> Int -> IO ()
insertBulkUsers conn n = do
  let values = BS8.intercalate ","
        [ "('user_" <> BS8.pack (show i) <> "', 'user" <> BS8.pack (show i) <> "@test.com')"
        | i <- [1 .. n]
        ]
      sql = "INSERT INTO users (name, email) VALUES " <> values
  _ <- simpleQuery conn sql
  pure ()

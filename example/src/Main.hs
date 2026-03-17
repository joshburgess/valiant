{-# LANGUAGE DeriveGeneric #-}

-- | Example REST API using hsqlx + scotty.
--
-- Demonstrates: connection pooling, typed queries, transactions,
-- pipelined batch inserts, and JSON serialization.
--
-- Run:
--   eval $(scripts/pg-setup.sh)
--   cabal run hsqlx-example
--   curl http://localhost:3000/users
module Main where

import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hsqlx
import Network.HTTP.Types.Status (status201, status404)
import Queries
import System.Environment (lookupEnv)
import Web.Scotty

main :: IO ()
main = do
  dbUrl <- lookupEnv "DATABASE_URL" >>= \case
    Just url -> pure (BS8.pack url)
    Nothing -> do
      putStrLn "DATABASE_URL not set, using default"
      pure "postgres://hsqlx_test:hsqlx_test@localhost:5433/hsqlx_test"

  -- Create connection pool
  pool <- newPool defaultPoolConfig
    { poolConnString = dbUrl
    , poolSize = 10
    }

  -- Set up schema
  withResource pool $ \conn -> do
    _ <- simpleQuery conn
      "CREATE TABLE IF NOT EXISTS users (\
      \  id SERIAL PRIMARY KEY,\
      \  name TEXT NOT NULL,\
      \  email TEXT,\
      \  created_at TIMESTAMPTZ NOT NULL DEFAULT now()\
      \)"
    _ <- simpleQuery conn
      "CREATE TABLE IF NOT EXISTS posts (\
      \  id SERIAL PRIMARY KEY,\
      \  author_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,\
      \  title TEXT NOT NULL,\
      \  body TEXT,\
      \  published_at TIMESTAMPTZ,\
      \  created_at TIMESTAMPTZ NOT NULL DEFAULT now()\
      \)"
    pure ()

  putStrLn "hsqlx example API running on http://localhost:3000"
  putStrLn ""
  putStrLn "  GET    /users              - list all users"
  putStrLn "  GET    /users/:id          - get user by id"
  putStrLn "  POST   /users              - create user (JSON: name, email)"
  putStrLn "  DELETE /users/:id          - delete user"
  putStrLn "  GET    /posts/recent?n=10  - list recent published posts"
  putStrLn "  GET    /posts/:id          - get post by id"
  putStrLn "  POST   /posts              - create post (JSON: author_id, title, body)"
  putStrLn "  POST   /posts/:id/publish  - publish a draft post"
  putStrLn "  POST   /seed               - seed sample data"

  scotty 3000 $ do
    -- ── Users ──────────────────────────────────────────────────────

    get "/users" $ do
      users <- liftIO $ withResource pool $ \conn ->
        fetchAll conn listUsers ()
      json [ object ["id" .= i, "name" .= n, "email" .= e]
           | (i, n, e) :: (Int32, Text, Maybe Text) <- users
           ]

    get "/users/:id" $ do
      uid <- pathParam "id" :: ActionM Int32
      mUser <- liftIO $ withResource pool $ \conn ->
        fetchOne conn findUserById uid
      case mUser of
        Nothing -> do
          status status404
          json (object ["error" .= ("User not found" :: Text)])
        Just (i, n, e, created) ->
          json $ object
            [ "id" .= i
            , "name" .= n
            , "email" .= e
            , "created_at" .= (created :: UTCTime)
            ]

    post "/users" $ do
      req <- jsonData :: ActionM CreateUserReq
      newId <- liftIO $ withResource pool $ \conn ->
        fetchScalar conn insertUser (cuName req, cuEmail req)
      status status201
      json $ object ["id" .= (newId :: Int32)]

    delete "/users/:id" $ do
      uid <- pathParam "id" :: ActionM Int32
      n <- liftIO $ withResource pool $ \conn ->
        execute conn deleteUser uid
      if n > 0
        then json $ object ["deleted" .= True]
        else do
          status status404
          json $ object ["error" .= ("User not found" :: Text)]

    -- ── Posts ──────────────────────────────────────────────────────

    get "/posts/recent" $ do
      n <- queryParamMaybe "n" >>= \case
        Just v -> pure v
        Nothing -> pure 10 :: ActionM Int32
      posts <- liftIO $ withResource pool $ \conn ->
        fetchAll conn listRecentPosts n
      json [ object [ "id" .= i, "title" .= t
                    , "author" .= a, "published_at" .= p ]
           | (i, t, a, p) :: (Int32, Text, Text, Maybe UTCTime) <- posts
           ]

    get "/posts/:id" $ do
      pid <- pathParam "id" :: ActionM Int32
      mPost <- liftIO $ withResource pool $ \conn ->
        fetchOne conn findPostById pid
      case mPost of
        Nothing -> do
          status status404
          json (object ["error" .= ("Post not found" :: Text)])
        Just (i, title, postBody, published, author) ->
          json $ object
            [ "id" .= i
            , "title" .= title
            , "body" .= (postBody :: Maybe Text)
            , "published_at" .= (published :: Maybe UTCTime)
            , "author" .= (author :: Text)
            ]

    post "/posts" $ do
      req <- jsonData :: ActionM CreatePostReq
      newId <- liftIO $ withResource pool $ \conn ->
        fetchScalar conn insertPost (cpAuthorId req, cpTitle req, cpBody req)
      status status201
      json $ object ["id" .= (newId :: Int32)]

    post "/posts/:id/publish" $ do
      pid <- pathParam "id" :: ActionM Int32
      n <- liftIO $ withResource pool $ \conn ->
        execute conn publishPost pid
      if n > 0
        then json $ object ["published" .= True]
        else do
          status status404
          json $ object ["error" .= ("Post not found or already published" :: Text)]

    -- ── Seed data ──────────────────────────────────────────────────

    post "/seed" $ do
      _ <- liftIO $ withTransaction pool $ \tx -> do
        let conn = txConn tx
        -- Batch-insert users using pipelining
        _ <- executeBatch conn insertUserBatch
          [ ("Alice", Just "alice@example.com")
          , ("Bob", Just "bob@example.com")
          , ("Carol", Nothing)
          ]
        -- Insert some posts
        _ <- fetchScalar conn insertPost (1 :: Int32, "First Post" :: Text, Just "Hello, world!" :: Maybe Text)
        _ <- fetchScalar conn insertPost (1 :: Int32, "Second Post" :: Text, Just "More content" :: Maybe Text)
        _ <- fetchScalar conn insertPost (2 :: Int32, "Bob's Draft" :: Text, Nothing :: Maybe Text)
        -- Publish the first two
        _ <- execute conn publishPost (1 :: Int32)
        _ <- execute conn publishPost (2 :: Int32)
        pure ()
      json $ object ["seeded" .= True]

-- Request types -------------------------------------------------------------

data CreateUserReq = CreateUserReq
  { cuName :: Text
  , cuEmail :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreatePostReq = CreatePostReq
  { cpAuthorId :: Int32
  , cpTitle :: Text
  , cpBody :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

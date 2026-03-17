-- | Query definitions for the example app.
--
-- Each binding references a @.sql@ file via 'queryFile'. The hsqlx GHC
-- plugin rewrites these to 'mkStatement' calls at compile time and
-- validates that the type signatures match the Postgres schema.
--
-- Run @hsqlx prepare@ whenever the SQL files or database schema change.
module Queries where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Hsqlx (Statement, queryFile)

-- Users ---------------------------------------------------------------------

-- | sql/users/find_by_id.sql
-- SELECT id, name, email, created_at FROM users WHERE id = $1
findUserById :: Statement Int32 (Int32, Text, Maybe Text, UTCTime)
findUserById = queryFile "users/find_by_id.sql"

-- | sql/users/list_all.sql
-- SELECT id, name, email FROM users ORDER BY id
listUsers :: Statement () (Int32, Text, Maybe Text)
listUsers = queryFile "users/list_all.sql"

-- | sql/users/insert.sql
-- INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id
insertUser :: Statement (Text, Maybe Text) Int32
insertUser = queryFile "users/insert.sql"

-- | sql/users/insert_batch.sql
-- INSERT INTO users (name, email) VALUES ($1, $2)
-- (no RETURNING — for use with executeBatch)
insertUserBatch :: Statement (Text, Maybe Text) ()
insertUserBatch = queryFile "users/insert_batch.sql"

-- | sql/users/delete.sql
-- DELETE FROM users WHERE id = $1
deleteUser :: Statement Int32 ()
deleteUser = queryFile "users/delete.sql"

-- Posts ---------------------------------------------------------------------

-- | sql/posts/find_by_id.sql
-- SELECT p.id, p.title, p.body, p.published_at, u.name as author_name
-- FROM posts p JOIN users u ON u.id = p.author_id WHERE p.id = $1
findPostById :: Statement Int32 (Int32, Text, Maybe Text, Maybe UTCTime, Text)
findPostById = queryFile "posts/find_by_id.sql"

-- | sql/posts/list_recent.sql
-- SELECT p.id, p.title, u.name, p.published_at ... LIMIT $1
listRecentPosts :: Statement Int64 (Int32, Text, Text, Maybe UTCTime)
listRecentPosts = queryFile "posts/list_recent.sql"

-- | sql/posts/insert.sql
-- INSERT INTO posts (author_id, title, body) VALUES ($1, $2, $3) RETURNING id
insertPost :: Statement (Int32, Text, Maybe Text) Int32
insertPost = queryFile "posts/insert.sql"

-- | sql/posts/publish.sql
-- UPDATE posts SET published_at = now() WHERE id = $1 AND published_at IS NULL
publishPost :: Statement Int32 ()
publishPost = queryFile "posts/publish.sql"

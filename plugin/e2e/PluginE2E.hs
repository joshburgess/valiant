-- | End-to-end test: this module compiles with the plugin loaded.
-- The plugin rewrites queryFile calls to mkStatement calls and validates types.
module PluginE2E where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import Valiant.Statement (Statement, queryFile)

-- | Positional params: SELECT 5 columns, 1 Int32 param.
-- sql/users/find_by_id.sql:
--   SELECT id, name, email, is_active, created_at FROM users WHERE id = $1
findUserById :: Statement Int32 (Int32, Text, Maybe Text, Bool, UTCTime)
findUserById = queryFile "users/find_by_id.sql"

-- | No params: SELECT 2 columns.
-- sql/users/list_all.sql:
--   SELECT id, name FROM users ORDER BY name
listUsers :: Statement () (Int32, Text)
listUsers = queryFile "users/list_all.sql"

-- | 2 params, no result columns (INSERT).
-- sql/users/insert.sql:
--   INSERT INTO users (name, email) VALUES ($1, $2)
insertUser :: Statement (Text, Maybe Text) ()
insertUser = queryFile "users/insert.sql"

-- | Named params: :name and :active mapped to $1 (Text) and $2 (Bool).
-- sql/users/find_by_name_and_status.sql:
--   SELECT id, name, email FROM users WHERE name = :name AND is_active = :active
findByNameAndStatus :: Statement (Text, Bool) (Int32, Text, Maybe Text)
findByNameAndStatus = queryFile "users/find_by_name_and_status.sql"

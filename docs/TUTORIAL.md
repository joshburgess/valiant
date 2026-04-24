# valiant Tutorial

A step-by-step guide to using valiant in a Haskell project.

## Prerequisites

- GHC 9.10
- Cabal 3.0+
- A running PostgreSQL instance

## 1. Add dependencies

In your `.cabal` file:

```cabal
build-depends:
  , valiant         >= 0.1
  , valiant-plugin  >= 0.1
```

## 2. Create your database schema

```sql
-- migrations/001_create_users.sql
CREATE TABLE users (
  id         SERIAL PRIMARY KEY,
  name       TEXT NOT NULL,
  email      TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
```

Apply it:

```bash
psql $DATABASE_URL -f migrations/001_create_users.sql
```

## 3. Write SQL files

Create a `sql/` directory at the project root. One SQL statement per file.

```sql
-- sql/users/find_by_id.sql
SELECT id, name, email, created_at
FROM users
WHERE id = $1
```

```sql
-- sql/users/list_all.sql
SELECT id, name FROM users ORDER BY name
```

```sql
-- sql/users/insert.sql
INSERT INTO users (name, email) VALUES ($1, $2)
```

```sql
-- sql/users/count.sql
SELECT count(*) FROM users
```

## 4. Run `valiant prepare`

This validates every `.sql` file against your database and writes type
metadata to `.valiant/`.

```bash
export DATABASE_URL="postgres://user:pass@localhost:5432/mydb"
valiant prepare
```

Commit the `.valiant/` directory to version control. This allows offline
compilation and CI builds without a database.

## 5. Write Haskell bindings

```haskell
{-# OPTIONS_GHC -fplugin=Valiant.Plugin
                -fplugin-opt=Valiant.Plugin:sql-dir=sql #-}

module MyApp.Queries.Users where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Valiant

-- The plugin verifies these types at compile time.

findById :: Statement Int32 (Int32, Text, Maybe Text, UTCTime)
findById = queryFile "users/find_by_id.sql"

listAll :: Statement () [(Int32, Text)]
listAll = queryFile "users/list_all.sql"

insert :: Statement (Text, Maybe Text) ()
insert = queryFile "users/insert.sql"

count :: Statement () Int64
count = queryFile "users/count.sql"
```

If a type is wrong, you get a compile error showing exactly which column
mismatched and what the correct type should be.

You can also skip the type signature entirely and let the plugin infer it,
or use a typed hole (`_ :: _`) to discover the type.

## 6. Use named result types

For richer domain types, derive `FromRow` via `Generic`:

```haskell
import GHC.Generics (Generic)

data User = User
  { userId    :: Int32
  , userName  :: Text
  , userEmail :: Maybe Text
  , userCreatedAt :: UTCTime
  } deriving (Generic, FromRow)

findById :: Statement Int32 User
findById = queryFileAs @User "users/find_by_id.sql"
```

The plugin checks that the fields match the SQL result columns by position.

## 7. Execute queries

```haskell
import Valiant
import MyApp.Queries.Users qualified as Q

main :: IO ()
main = do
  pool <- newPool defaultPoolConfig
    { poolConnString = "postgres://user:pass@localhost:5432/mydb"
    , poolSize = 10
    }

  -- Fetch one row (Maybe)
  mUser <- withResource pool $ \conn ->
    fetchOne conn Q.findById 42

  -- Fetch all rows
  users <- withResource pool $ \conn ->
    fetchAll conn Q.listAll ()

  -- Scalar query
  n <- withResource pool $ \conn ->
    fetchScalar conn Q.count ()

  -- Insert
  withResource pool $ \conn ->
    execute conn Q.insert ("Alice", Just "alice@example.com")

  closePool pool
```

## 8. Transactions

```haskell
withTransaction pool $ \tx -> do
  execute (txConn tx) Q.insert ("Bob", Just "bob@example.com")
  execute (txConn tx) Q.insert ("Carol", Nothing)
  -- If an exception is thrown, the transaction is rolled back.

-- Read-only transaction (safe for replicas)
withReadOnlyTransaction pool $ \tx -> do
  users <- fetchAll (txConn tx) Q.listAll ()
  pure users

-- Custom isolation level
withTransactionLevel Serializable pool $ \tx -> do
  ...

-- Nested savepoints
withTransaction pool $ \tx -> do
  execute (txConn tx) Q.insert ("Dave", Nothing)
  withSavepoint tx $ \sp -> do
    execute (txConn sp) Q.insert ("Eve", Nothing)
    -- If this throws, only the savepoint is rolled back.
```

## 9. Batch writes

`executeBatch` sends all statements in a single pipelined round-trip
(40-100x faster than sequential inserts):

```haskell
withResource pool $ \conn ->
  executeBatch conn Q.insert
    [ ("Alice", Just "alice@example.com")
    , ("Bob",   Just "bob@example.com")
    , ("Carol", Nothing)
    ]
```

## 10. Pipelined reads

Combine independent queries into one network round-trip with `Pipeline`:

```haskell
(user, posts) <- withResource pool $ \conn ->
  runPipeline conn $
    (,) <$> pipeFetchOne conn Q.findUserById 42
        <*> pipeFetchAll conn Q.listPostsByUser 42
```

## 11. Streaming large result sets

For constant-memory processing without loading all rows:

```haskell
-- RowFold: process rows as they arrive, no intermediate list
total <- withResource pool $ \conn ->
  executeWithFold conn Q.listAll () $
    RowFold 0 (\acc _row -> acc + 1)

-- Server-side cursor for batch-at-a-time processing
withTransaction pool $ \tx ->
  withCursor (txConn tx) Q.listAll () 100 $ \cursor -> do
    batch <- fetchBatch cursor
    -- process batch...
```

## 12. COPY for bulk data

```haskell
-- COPY IN from CSV
copyIn conn "COPY users (name, email) FROM STDIN WITH (FORMAT csv)" $
  [ "Alice,alice@example.com\n"
  , "Bob,bob@example.com\n"
  ]

-- COPY OUT
rows <- copyOut conn "COPY users TO STDOUT WITH (FORMAT csv)"
```

## 13. LISTEN/NOTIFY

```haskell
withResource pool $ \conn -> do
  listen conn "user_events"
  waitForNotification conn $ \notif ->
    putStrLn $ "Channel: " <> show (nChannel notif)
              <> " Payload: " <> show (nPayload notif)
```

## 14. The Valiant monad

For convenience, `Valiant` is a `ReaderT Pool IO` monad that threads the
pool implicitly:

```haskell
app :: Valiant ()
app = do
  users <- fetchAllM Q.listAll ()
  withTransactionM $ \tx ->
    executeM Q.insert ("NewUser", Nothing)

main :: IO ()
main = do
  pool <- newPool defaultPoolConfig { poolConnString = "..." }
  runValiant pool app
```

## 15. Connection pool tuning

```haskell
pool <- newPool defaultPoolConfig
  { poolConnString     = "postgres://..."
  , poolSize           = 20              -- max connections
  , poolIdleTime       = 600             -- close idle after 10 min
  , poolMaxLife        = 3600            -- max connection lifetime 1 hour
  , poolMaxLifeJitter  = 60             -- ±60s jitter to avoid thundering herd
  , poolAcquireTimeout = 10              -- throw PoolTimeout after 10s
  , poolMinIdle        = 5               -- warm pool with 5 connections
  , poolRecyclingMethod = RecycleVerified -- health-check idle connections
  , poolQueueMode      = QueueLIFO       -- reuse newest connections first
  }

-- Lifecycle hooks
setPostCreateHook pool $ \conn ->
  simpleQuery conn "SET search_path TO myapp, public"

setPreReleaseHook pool $ \conn ->
  simpleQuery conn "RESET ALL"

-- Runtime stats
stats <- poolStats pool
print (psInUse stats, psIdle stats, psWaiters stats)

-- Dynamic resize
resize pool 30
```

## 16. Custom type mappings

By default, valiant maps standard Postgres types to Haskell types. For
custom types (enums, domains, composite types), create `valiant-types.json`:

```json
{
  "types": {
    "user_role": {
      "haskell_type": "UserRole",
      "haskell_module": "MyApp.Types"
    }
  }
}
```

For Postgres enums, derive `PgEnum`:

```haskell
data UserRole = Admin | Editor | Viewer
  deriving (Show, Eq)

instance PgEnum UserRole where
  pgEnumToText Admin  = "admin"
  pgEnumToText Editor = "editor"
  pgEnumToText Viewer = "viewer"
  pgEnumFromText "admin"  = Just Admin
  pgEnumFromText "editor" = Just Editor
  pgEnumFromText "viewer" = Just Viewer
  pgEnumFromText _        = Nothing
```

## 17. Code generation

Instead of writing bindings by hand, generate them:

```bash
valiant generate \
  --module-prefix MyApp.Queries \
  --output-dir src/MyApp/Queries/
```

This creates one module per `sql/` subdirectory with inferred types. Edit
the generated files freely. The plugin continues to verify everything.

## 18. CI setup

```yaml
steps:
  # No database needed, just check the cache is current
  - name: Verify query cache
    run: valiant check

  # Build in offline mode
  - name: Build
    run: cabal build
    env:
      VALIANT_OFFLINE: "true"

  # Tests need a database
  - name: Test
    run: cabal test
    env:
      DATABASE_URL: ${{ secrets.TEST_DATABASE_URL }}
```

## 19. Error messages

valiant produces clear, actionable compile errors. Examples:

**Wrong column type:**
```
error: [VALIANT-003]
    Column    Postgres type    Your type       Expected
    email     text (nullable)  Text            Maybe Text  MISMATCH

    Fix: wrap the field in Maybe:  Maybe Text
```

**Wrong parameter count:**
```
error: [VALIANT-006]
    Your type provides 1 parameter but the query expects 2.
    Fix: change parameter type to a 2-tuple: Statement (Int32, Text) [...]
```

**File not found:**
```
error: [VALIANT-001]
    sql/users/find_by_idd.sql not found.
    Did you mean: sql/users/find_by_id.sql (edit distance: 1)
```

## 20. File naming conventions

`valiant generate` infers return types from file name prefixes:

| Prefix | Generated return type |
|--------|---------------------|
| `find_*` | `Statement p (Maybe r)` |
| `get_*` | `Statement p r` |
| `list_*` | `Statement p [r]` |
| `count_*` | `Statement p Int64` |
| `exists_*` | `Statement p Bool` |
| `insert*`, `update_*`, `delete_*` | `Statement p ()` |

These are conventions, not requirements. You can use any names and write
your own type signatures.

## Next steps

- See the [example project](../example/) for a complete REST API using valiant + scotty
- See [PERFORMANCE.md](PERFORMANCE.md) for benchmarks and optimization details
- See [ASYNC_ARCHITECTURE.md](ASYNC_ARCHITECTURE.md) for the sender/receiver split design
- Run `valiant --help` for all CLI options

# hsqlx

**Compile-time checked SQL for Haskell.**

A faithful recreation of Rust's [sqlx](https://github.com/launchbadge/sqlx), built from scratch for Haskell. No Template Haskell. No `hasql`. Raw `.sql` files validated against a live Postgres database at prepare time, with a GHC source plugin that enforces type safety at compile time.

## How it works

```
  .sql files ──> hsqlx prepare ──> .hsqlx/ cache ──> GHC plugin ──> type-safe Haskell
                    (live DB)        (committed)      (compile time)
```

1. Write SQL in standalone `.sql` files with full editor support
2. Run `hsqlx prepare` to validate queries against your database and cache type metadata
3. The GHC source plugin reads the cache at compile time and verifies your Haskell types match
4. At runtime, a custom Postgres wire protocol driver executes queries with binary format encoding

## Quick start

```haskell
-- sql/users/find_by_id.sql:
--   SELECT id, name, email FROM users WHERE id = $1

{-# OPTIONS_GHC -fplugin=Hsqlx.Plugin
                -fplugin-opt=Hsqlx.Plugin:sql-dir=sql #-}

module MyApp.Queries.Users where

import Hsqlx

findById :: Statement Int32 (Maybe (Int32, Text, Maybe Text))
findById = queryFile "users/find_by_id.sql"
```

The plugin verifies at compile time that:
- The `.sql` file exists
- `Int32` matches the `$1` parameter (Postgres `int4`)
- `(Int32, Text, Maybe Text)` matches the result columns
- `email` is correctly wrapped in `Maybe` (it's nullable)

If anything is wrong, you get a clear compile error:

```
src/MyApp/Queries/Users.hs:12:1: error: [HSQLX-003]

    -- Result type mismatch
    |
    |  Comparing column by column:
    |
    |    Column    Postgres type    Your type       Expected
    |    id        int4             Int32           Int32       ok
    |    name      text             Text            Text        ok
    |    email     text (nullable)  Text            Maybe Text  MISMATCH
    |
    -- Fix: wrap the field in Maybe:  Maybe Text
```

## Runtime usage

```haskell
import Hsqlx
import MyApp.Queries.Users qualified as Q

main :: IO ()
main = do
  pool <- newPool defaultPoolConfig
    { poolConnString = "postgres://user:pass@localhost:5432/mydb"
    , poolSize = 10
    }

  -- Fetch one row
  mUser <- withResource pool $ \conn ->
    fetchOne conn Q.findById 42

  -- Fetch all rows
  users <- withResource pool $ \conn ->
    fetchAll conn Q.listAll ()

  -- Execute a command
  n <- withResource pool $ \conn ->
    execute conn Q.insert ("Alice", Just "alice@example.com")

  -- Batch insert (pipelined — eliminates per-row round-trips)
  withResource pool $ \conn ->
    executeBatch conn Q.insert
      [ ("Alice", Just "alice@example.com")
      , ("Bob", Just "bob@example.com")
      , ("Carol", Nothing)
      ]

  -- Transactions
  withTransaction pool $ \tx -> do
    execute (txConn tx) Q.insert ("Dave", Just "dave@example.com")
    execute (txConn tx) Q.insert ("Eve", Nothing)
```

## CLI tool

```bash
# Validate all .sql files against your database
$ hsqlx prepare
  [1/10] sql/users/find_by_id.sql ............ ok
  [2/10] sql/users/find_by_email.sql ......... ok
  ...
  Wrote 10 cache files to .hsqlx/

# Check cache freshness (for CI, no database needed)
$ hsqlx check

# Print inferred Haskell types
$ hsqlx types
  sql/users/find_by_id.sql
    Params: Int32
    Result: (Int32, Text, Maybe Text)

# Auto-generate Haskell binding modules
$ hsqlx generate --module-prefix MyApp.Queries --output-dir src/MyApp/Queries/
  Generated src/MyApp/Queries/Users.hs (7 queries)
  Generated src/MyApp/Queries/Posts.hs (3 queries)

# Watch for changes and re-prepare
$ hsqlx watch
```

## Performance

hsqlx is the fastest Haskell PostgreSQL library. It implements its own wire
protocol in pure Haskell with binary format encoding, direct byte writes,
pipelined execution, and zero unnecessary copies.

Benchmarks against [hasql](https://hackage.haskell.org/package/hasql)
(libpq FFI, binary) and [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)
(libpq FFI, text). Single connection, Docker Postgres 16.

**Reads:**

| Rows | hsqlx | hasql | pg-simple | vs hasql | vs pg-simple |
|------|-------|-------|-----------|----------|--------------|
| 1 (by PK) | 0.97 ms | 0.99 ms | 1.06 ms | **faster** | **9% faster** |
| 1,000 | 4.5 ms | 7.4 ms | 8.3 ms | **39% faster** | **46% faster** |
| 5,000 | 19.6 ms | 37.5 ms | 41.5 ms | **48% faster** | **53% faster** |
| 10,000 | 37.2 ms | 74.5 ms | 81.7 ms | **50% faster** | **54% faster** |

**Writes (pipelined):**

| Rows | hsqlx (pipelined) | hasql | pg-simple |
|------|--------------------|-------|-----------|
| 100 | **2.5 ms** | 104 ms | 115 ms |
| 1,000 | **11.7 ms** | 926 ms | 1.20 s |
| 5,000 | **48.8 ms** | 4.92 s | 4.84 s |

**Why it's fast:**

- **Binary format decoding** — direct from network buffer, no FFI boundary
- **Direct byte writes** — `unsafeCreate` + `pokeByteOff`, 5-6x faster than Builder for fixed-size types (30ns per Int32)
- **Pipelined execution** — `executeBatch` sends N Bind+Execute pairs with 1 Sync, 40-100x faster than sequential
- **Pre-computed message sizes** — single-pass encoding, no double-copy
- **Message coalescing + TCP_NODELAY** — single syscall per message sequence
- **Fused row decoding** — decode as rows arrive, no intermediate list
- **Pure Haskell** — no `libpq`, no C toolchain, no system dependencies

| | libpq (FFI) | hsqlx (pure Haskell) |
|---|---|---|
| Single-row latency | Baseline | **Matching** (0.97ms vs 0.99ms) |
| Multi-row throughput | Baseline | **2x faster** at 10K rows |
| Batch writes | No pipelining | **40-100x faster** |
| Build requirements | Needs `libpq-dev` | No system dependencies |

See [docs/PERFORMANCE.md](docs/PERFORMANCE.md) for the full deep-dive:
codec benchmarks, architecture comparison, optimization techniques, and
the complete optimization journey from 177ns to 30ns Int32 encoding.

## Project structure

hsqlx is a multi-package Cabal project:

| Package | Description |
|---------|-------------|
| `hsqlx-cli` | CLI tool (`hsqlx prepare`, `check`, `types`, `generate`, `watch`) |
| `hsqlx` | Runtime library: custom PG wire protocol driver, connection pool, binary codecs |
| `hsqlx-plugin` | GHC source plugin for compile-time query validation |
| `bench-compare` | Comparative benchmarks against hasql and postgresql-simple |

```
hsqlx/
├── src/                  # hsqlx-cli source
├── app/                  # CLI executable entry point
├── runtime/              # hsqlx runtime library
│   ├── src/Hsqlx/        # Wire protocol, codecs, pool, etc.
│   ├── bench/            # Codec benchmarks (criterion)
│   └── integration/      # Integration tests (require Postgres)
├── plugin/               # GHC source plugin
│   └── src/Hsqlx/Plugin/ # AST traversal, verification, errors
├── bench-compare/        # Comparative benchmarks vs hasql, pg-simple
├── scripts/              # pg-setup.sh, pg-teardown.sh
├── sql/                  # Example .sql files
├── .hsqlx/               # Cached query metadata (committed to VCS)
└── test/                 # Test suites for all packages
```

## Features

### SQL authoring
- One SQL statement per `.sql` file with full editor support
- Optional metadata comments: `-- hsqlx:name`, `-- hsqlx:result`, `-- hsqlx:single`
- Directory structure maps to Haskell module structure

### Compile-time validation
- 9 structured error types with column-by-column diagnostics
- Nullability inference from `pg_attribute`
- Did-you-mean suggestions for mistyped file paths (Levenshtein distance)
- Type inference (no signature required) or typed hole discovery
- `addDependentFile` tracking: GHC recompiles when `.sql` files change

### Type mapping

| Postgres | Haskell | Postgres | Haskell |
|----------|---------|----------|---------|
| `bool` | `Bool` | `float4` | `Float` |
| `int2` | `Int16` | `float8` | `Double` |
| `int4` | `Int32` | `numeric` | `Scientific` |
| `int8` | `Int64` | `uuid` | `UUID` |
| `text` | `Text` | `json`/`jsonb` | `Value` |
| `bytea` | `ByteString` | `date` | `Day` |
| `varchar` | `Text` | `time` | `TimeOfDay` |
| `timestamp` | `LocalTime` | `timestamptz` | `UTCTime` |
| `interval` | `PgInterval` | `int4[]`, etc. | `Vector Int32`, etc. |

Nullable columns are wrapped in `Maybe`. Unknown OIDs can be registered via `hsqlx-types.json`.

### Runtime
- Custom PostgreSQL v3 wire protocol implementation (no FFI, no `libpq`)
- Binary format encoding/decoding for all supported types
- Extended query protocol (Parse/Bind/Execute/Sync) with prepared statement caching
- Pipelined batch execution (`executeBatch`) for high-throughput writes
- Message coalescing and `TCP_NODELAY` for minimal per-message overhead
- Connection pooling with idle reaping, max lifetime, and health checking
- SCRAM-SHA-256, MD5, and cleartext authentication
- TLS support via the `tls` library
- Transactions with configurable isolation levels
- Streaming results via server-side cursors
- LISTEN/NOTIFY for async notifications
- COPY IN/OUT for bulk data transfer
- Composite and range type binary codecs
- Logging hooks for query timing and connection events

## Workflow

### Development

```bash
# 1. Write a query
echo "SELECT id, name FROM users WHERE active = true" > sql/users/list_active.sql

# 2. Validate against your dev database
export DATABASE_URL="postgres://localhost:5432/mydb"
hsqlx prepare

# 3. Write (or generate) the Haskell binding
# 4. Build — the plugin checks everything at compile time
cabal build
```

### CI

```yaml
steps:
  - name: Verify query cache
    run: hsqlx check          # no database needed

  - name: Build
    run: cabal build
    env:
      HSQLX_OFFLINE: "true"   # plugin reads from .hsqlx/ only
```

### Running benchmarks

```bash
# Start a test Postgres instance (Docker) or set DATABASE_URL
eval $(scripts/pg-setup.sh)

# Codec benchmarks (pure, no database needed)
cabal bench hsqlx-bench

# Comparative benchmarks vs hasql and postgresql-simple
cabal run bench-compare

# Teardown
scripts/pg-teardown.sh
```

## Building from source

Requires GHC 9.4 and Cabal 3.0+.

```bash
git clone https://github.com/joshburgess/hsqlx.git
cd hsqlx
cabal build all
cabal test all    # 461 tests
```

## Design decisions

**Why not Template Haskell?** TH has stage restrictions, cross-compilation issues, and makes it hard to produce good error messages. A GHC source plugin runs after typechecking, has access to the full AST, and can emit rich diagnostics with source locations and custom formatting.

**Why a separate prepare step?** Connecting to Postgres from inside the compiler (as Rust's sqlx does) causes well-known compilation speed issues and complicates CI. A separate CLI step + JSON cache keeps compilation fast and enables fully offline builds.

**Why a custom wire protocol driver?** Full control over binary format encoding, connection management, and protocol features (pipelining, COPY, LISTEN/NOTIFY, cursors) without depending on `libpq` or any existing Haskell database library. No system C dependencies means simpler builds and cross-compilation. And as the benchmarks show, pure Haskell binary decoding is faster than FFI-based alternatives on multi-row reads.

## Comparison with Rust's sqlx

| Aspect | Rust sqlx | hsqlx |
|--------|-----------|-------|
| SQL authoring | String literals or `.sql` files | `.sql` files (primary) |
| Compile-time mechanism | Proc macro | GHC source plugin |
| DB at compile time | From proc macro | Separate `hsqlx prepare` step |
| Offline mode | `.sqlx/` JSON cache | `.hsqlx/` JSON cache |
| Code generation | No | `hsqlx generate` (optional) |
| Runtime driver | Custom async Rust driver | Custom Haskell driver |
| Error messages | Generic Rust type errors | Column-by-column diagnostics with fixes |
| Pipelining | Via driver internals | Explicit `executeBatch` API |

## License

BSD-3-Clause

# hsqlx API Gap Analysis vs Competitors

Competitive analysis of hsqlx's public API against hasql, postgresql-simple,
and the higher-level type-safe libraries (rel8, beam, opaleye, squeal).

Last updated: 2026-03-17

## Methodology

Compared the full export lists of:
- **hasql** 1.10.3 + hasql-pool 1.4.2 + hasql-transaction 1.2.2 + hasql-dynamic-statements
- **postgresql-simple** 0.7.0.0 (all 27 public modules)
- **rel8**, **beam** / beam-postgres, **opaleye**, **squeal-postgresql** (API patterns and execution features)

Since hsqlx uses raw `.sql` files, users automatically get access to all
PostgreSQL query features (CTEs, window functions, lateral joins, DISTINCT ON,
etc.) without needing Haskell combinators. The gaps are on the **execution,
error handling, and type codec** sides, not query construction.

---

## Tier 1: High Priority

These are production blockers or things users will hit immediately.

### 1.1 Constraint Violation Helpers

**Gap:** We expose raw `QueryError PgError` with no structured helpers.
Every production app needs to catch unique constraint violations, foreign
key violations, etc.

**postgresql-simple provides:**
```haskell
data ConstraintViolation
  = NotNullViolation ByteString
  | ForeignKeyViolation ByteString
  | UniqueViolation ByteString
  | CheckViolation ByteString
  | ExclusionViolation ByteString

constraintViolation  :: SqlError -> Maybe ConstraintViolation
constraintViolationE :: SqlError -> Maybe (SqlError, ConstraintViolation)
catchViolation       :: (SqlError -> ConstraintViolation -> IO a) -> IO a -> IO a
isSerializationError      :: SqlError -> Bool
isNoActiveTransactionError :: SqlError -> Bool
isFailedTransactionError  :: SqlError -> Bool
```

**Recommendation:** Add `Hsqlx.Error` module with:
- `ConstraintViolation` ADT
- `constraintViolation :: HsqlxError -> Maybe ConstraintViolation`
- `catchConstraintViolation :: (HsqlxError -> ConstraintViolation -> IO a) -> IO a -> IO a`
- `sqlState :: HsqlxError -> Maybe ByteString`
- `isUniqueViolation`, `isForeignKeyViolation`, `isSerializationError`, etc.
- Pattern synonyms or SQLSTATE constants for common error codes

### 1.2 Transaction Retry on Serialization Failure

**Gap:** No built-in retry mechanism for serializable transactions.

**postgresql-simple provides:**
```haskell
withTransactionModeRetry :: TransactionMode -> (SqlError -> Bool) -> Connection -> IO a -> IO a
```
The predicate-based design is more flexible than hardcoding serialization errors.

**hasql-transaction** automatically retries on SQLSTATE 40001 when using
`Serializable` isolation.

**Recommendation:** Add:
- `withTransactionRetry :: Int -> Pool -> (Transaction -> IO a) -> IO a`
- `withTransactionLevelRetry :: IsolationLevel -> Int -> Pool -> (Transaction -> IO a) -> IO a`
- `withTransactionRetryIf :: (HsqlxError -> Bool) -> Int -> Pool -> (Transaction -> IO a) -> IO a`
- Corresponding `*M` variants for the Hsqlx monad

### 1.3 Vector Return Variants

**Gap:** All query functions return `[r]`. Building a list then converting
to Vector is wasteful for large result sets.

**hasql** returns `Vector` by default via `rowVector :: Row a -> Result (Vector a)`.
**postgresql-simple** has a dedicated `Database.PostgreSQL.Simple.Vector` module
with `query`, `query_`, `returning`, etc. returning `Vector r`.

**Recommendation:** Add:
- `fetchAllVec :: Connection -> Statement p r -> p -> IO (Vector r)`
- `fetchAllVecM :: Statement p r -> p -> Hsqlx (Vector r)`
- Internally, decode directly into a mutable vector instead of building a list

### 1.4 forEach (Streaming Callback)

**Gap:** We have `RowFold` for constant-memory processing and cursors for
batched streaming, but no simple callback interface.

**postgresql-simple provides:**
```haskell
forEach  :: (ToRow q, FromRow r) => Connection -> Query -> q -> (r -> IO ()) -> IO ()
forEach_ :: FromRow r => Connection -> Query -> (r -> IO ()) -> IO ()
```

**Recommendation:** Add:
- `forEach :: Connection -> Statement p r -> p -> (r -> IO ()) -> IO ()`
- `forEachM :: Statement p r -> p -> (r -> IO ()) -> Hsqlx ()`

### 1.5 Batch INSERT ... RETURNING (Pipelined)

**Gap:** We have `executeReturning` for a single parameter set, and
`executeBatch` for batch execution without RETURNING. No way to do a
pipelined batch insert that collects RETURNING results.

**All four higher-level libraries** (rel8, beam, opaleye, squeal) support
RETURNING on bulk inserts. beam-postgres even has `streamingRunInsertReturning`.

**Recommendation:** Add:
- `executeReturningMany :: Connection -> Statement p r -> [p] -> IO (Int64, [r])`
- Pipelined: send N Bind+Execute pairs, collect all RETURNING rows
- `executeReturningManyM` variant

### 1.6 Connection-Level Transaction

**Gap:** Our transaction functions take `Pool`, not `Connection`. Users who
already have a connection (from `withResource`) cannot easily start a
transaction without going through the pool again.

**postgresql-simple** takes `Connection` for all transaction functions.

**Recommendation:** Add:
- `withTransactionConn :: Connection -> (Transaction -> IO a) -> IO a`
- `withTransactionLevelConn :: IsolationLevel -> Connection -> (Transaction -> IO a) -> IO a`
- Keep the `Pool`-based variants as the primary API

---

## Tier 2: Medium Priority

Ergonomics and completeness for production use.

### 2.1 hstore Binary Codec

**Gap:** No hstore support. postgresql-simple has full support with
`HStoreMap`, `HStoreList`, `ToHStore`, `ToHStoreText`.

**Recommendation:** Add `Hsqlx.Binary.HStore` with `PgHStore` type
(newtype over `Map Text Text`), PgEncode/PgDecode instances. Register
OID in CLI type map.

### 2.2 macaddr / macaddr8 Binary Codec

**Gap:** No MAC address support. Pairs naturally with inet/cidr we just added.

**Recommendation:** Add `PgMacAddr` type with 6-byte (macaddr, OID 829)
and 8-byte (macaddr8, OID 774) codecs.

### 2.3 Timestamp Infinity Support

**Gap:** PostgreSQL supports `-infinity` and `infinity` as valid timestamp
and date values. Our binary codecs will decode these as extreme dates
rather than representing them distinctly.

**postgresql-simple provides:**
```haskell
data Unbounded a = NegInfinity | Finite a | PosInfinity
type Date = Unbounded Day
type UTCTimestamp = Unbounded UTCTime
```

**Recommendation:** Add `Unbounded a` type or `PgTimestamp` / `PgDate`
wrappers that can represent infinity. The binary format uses sentinel
values (INT64_MIN for -infinity, INT64_MAX for infinity).

### 2.4 Convenience Query Functions

**Gap:** Missing common patterns that reduce boilerplate.

**Recommendation:** Add:
- `fetchOneOr :: Connection -> Statement p r -> p -> r -> IO r` — return default on no rows
- `fetchFirst :: Connection -> Statement p r -> p -> IO (Maybe r)` — first row of multi-row query
- Both with `*M` monad variants

### 2.5 TransactionMode Record and Deferrable Transactions

**Gap:** No support for `BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE READ ONLY DEFERRABLE`.
This is important for long-running analytics queries on replicas — PostgreSQL
guarantees a consistent snapshot without risk of serialization failure.

**squeal-postgresql** has the most complete model: `IsolationLevel` ×
`AccessMode` × `DeferrableMode`.

**postgresql-simple provides:**
```haskell
data TransactionMode = TransactionMode
  { isolationLevel :: IsolationLevel
  , readWriteMode  :: ReadWriteMode
  }
```

**Recommendation:** Add `TransactionMode` record:
```haskell
data TransactionMode = TransactionMode
  { tmIsolation  :: IsolationLevel
  , tmReadOnly   :: Bool
  , tmDeferrable :: Bool
  }
```
With `withTransactionMode :: TransactionMode -> Pool -> (Transaction -> IO a) -> IO a`

### 2.6 Advisory Lock Helpers

**Gap:** `SELECT pg_advisory_xact_lock(key)` is a standard production
pattern for distributed coordination.

**Recommendation:** Add:
- `withAdvisoryLock :: Connection -> Int64 -> IO a -> IO a`
- `withAdvisoryLockTry :: Connection -> Int64 -> IO a -> IO (Maybe a)`
- Transaction-scoped variants that use `pg_advisory_xact_lock`

### 2.7 Pool from Connection String

**Gap:** Must construct `PoolConfig` record manually. Common to just have
a connection string.

**Recommendation:** Add:
- `newPoolFromString :: ByteString -> IO Pool` — uses `defaultPoolConfig` + string
- `newPoolFromStringWith :: ByteString -> (PoolConfig -> PoolConfig) -> IO Pool`

### 2.8 SQLSTATE Accessor on Errors

**Gap:** `HsqlxError` exposes `QueryError PgError` but there's no easy
way to extract the SQLSTATE code for programmatic error handling.

**Recommendation:** Add:
- `sqlState :: HsqlxError -> Maybe ByteString`
- `pgErrorField :: HsqlxError -> (PgError -> a) -> Maybe a`

---

## Tier 3: Low Priority

Nice to have, uncommon use cases.

### 3.1 Large Object Support

postgresql-simple has a complete large objects API: `loCreat`, `loOpen`,
`loRead`, `loWrite`, `loSeek`, `loTell`, `loTruncate`, `loClose`,
`loUnlink`, `loImport`, `loExport`. Uncommon in modern applications but
some legacy systems depend on it.

### 3.2 Geometric Type Codecs

`point` (OID 600), `line`, `box`, `path`, `polygon`, `circle`. Uncommon
outside GIS applications, and PostGIS users typically use extension types.

### 3.3 ZonedTime for timestamptz

hasql and postgresql-simple both support `ZonedTime` as an alternative
decode target for `timestamptz`. We only map to `UTCTime`. Adding
`ZonedTime` as an alternative would help users who need timezone-aware
display without manual conversion.

### 3.4 timetz Type

hasql supports `(TimeOfDay, TimeZone)` for the PG `timetz` type. This
type is uncommon (the PostgreSQL docs recommend against it) but exists
in some schemas.

### 3.5 Dynamic Statement Builder

For cases like dynamic filtering, optional WHERE clauses, dynamic
ORDER BY. hasql-dynamic-statements provides a `Snippet` type for this.
Less relevant for hsqlx since users can write multiple `.sql` files,
but some use cases genuinely need dynamic SQL composition.

### 3.6 money Type

PG `money` type (OID 790). Uncommon — the PostgreSQL docs recommend
using `numeric` instead. Users can `CAST(amount AS numeric)` in SQL.

### 3.7 Conduit / Streaming Library Integration

beam-postgres provides `streamingRunSelect` via Conduit. We have
`RowFold` and cursors which cover the use case differently (and arguably
better for most cases), but some users prefer streaming abstractions.

### 3.8 Tuples Beyond 10

postgresql-simple supports FromRow/ToRow for tuples up to 20 elements.
We support up to 10. Extending to 16 would cover most realistic schemas.

### 3.9 refine Decoder Combinator

hasql has `refine :: (a -> Either Text b) -> Value a -> Value b` for
post-decode validation. Less relevant for us since decoders are
plugin-generated, but could be useful for custom types.

---

## Existing Strengths (No Gap)

Features where hsqlx matches or exceeds all competitors:

| Feature | Notes |
|---------|-------|
| Compile-time SQL validation | Unique to hsqlx (via GHC plugin) |
| Binary protocol | hasql uses libpq binary; pg-simple uses text; we use our own binary |
| Pipelined batch writes | 40-100x faster than competitors |
| Pipeline Applicative | Combines independent reads in one round-trip |
| Server-side cursors | Built-in `withCursor` + `fetchBatch` |
| Constant-memory fold | `RowFold` / `executeWithFold` |
| COPY IN/OUT | Text, CSV, and binary formats |
| LISTEN/NOTIFY | `listen`, `unlisten`, `waitForNotification` with timeout |
| Connection pool | Health checking, hooks, jitter, reaping, min-idle warming |
| Async sender/receiver | 7.2x throughput scaling at 32 green threads |
| Query cancellation | `cancelQuery`, `withQueryTimeout` |
| Named parameters | `mkStatementNamed`, `ToNamedParams` |
| Generic FromRow/ToParams | Derive via `Generic`, no manual wiring |
| Read-only transactions | `withReadOnlyTransaction` |
| Savepoints | `withSavepoint` (nested) |
| TLS 1.2/1.3 | Client certs, CA validation, full hostname verification |
| Multi-host failover | `target_session_attrs`, `load_balance_hosts` |
| SCRAM-SHA-256 auth | Required by modern PostgreSQL |
| No C dependencies | Pure Haskell — no libpq, no system deps |
| Structured error messages | 9 error types with column-by-column diagnostics |

---

## Comparison Matrix

| Feature | hsqlx | hasql | pg-simple | Notes |
|---------|:-----:|:-----:|:---------:|-------|
| Compile-time SQL checking | **Yes** | No | No | GHC plugin |
| Binary wire protocol | **Yes** | Yes (libpq) | No (text) | |
| Pipelined batching | **Yes** | Yes (Pipeline) | No | |
| Connection pool | **Built-in** | Separate pkg | No | |
| Transactions | **Built-in** | Separate pkg | Built-in | |
| Transaction retry | **No** | Auto (serializable) | Yes | Gap |
| Constraint violation helpers | **No** | No | Yes | Gap |
| Vector result return | **No** | Yes (default) | Yes (module) | Gap |
| forEach callback | **No** | No | Yes | Gap |
| Batch RETURNING | **No** | No | Yes (`returning`) | Gap |
| Connection-level txn | **No** | Yes | Yes | Gap |
| hstore codec | **No** | No | Yes | Gap |
| macaddr codec | **No** | No | No | |
| Timestamp infinity | **No** | No | Yes | Gap |
| Large objects | **No** | No | Yes | |
| inet/cidr | **Yes** | Yes (iproute) | No | |
| Generic row derivation | **Yes** | No (manual) | No (manual) | |
| Streaming cursors | **Yes** | Manual only | Yes (fold) | |
| COPY protocol | **Yes** | No | Yes | |
| LISTEN/NOTIFY | **Yes** | Separate pkg | Partial | |
| Constant-memory fold | **Yes** | Yes (foldlRows) | Yes (fold) | |
| No system C deps | **Yes** | No (libpq) | No (libpq) | |

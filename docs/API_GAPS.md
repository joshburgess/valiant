# valiant API Gap Analysis vs Competitors

Competitive analysis of valiant's public API against hasql, postgresql-simple,
and the higher-level type-safe libraries (rel8, beam, opaleye, squeal).

Last updated: 2026-03-24

## Methodology

Compared the full export lists of:
- **hasql** 1.10.3 + hasql-pool 1.4.2 + hasql-transaction 1.2.2 + hasql-dynamic-statements
- **postgresql-simple** 0.7.0.0 (all 27 public modules)
- **rel8**, **beam** / beam-postgres, **opaleye**, **squeal-postgresql** (API patterns and execution features)

Since valiant uses raw `.sql` files, users automatically get access to all
PostgreSQL query features (CTEs, window functions, lateral joins, DISTINCT ON,
etc.) without needing Haskell combinators. The gaps are on the **execution,
error handling, and type codec** sides, not query construction.

---

## Tier 1: High Priority

These are production blockers or things users will hit immediately.

### ~~1.1 Constraint Violation Helpers~~ DONE

Implemented in `Valiant.Error` with `ConstraintViolation` ADT, `sqlState`,
`isUniqueViolation`, `isSerializationError`, etc.

### ~~1.2 Transaction Retry on Serialization Failure~~ DONE

Implemented as `withTransactionRetry` and `withTransactionRetryIf` in
`Valiant.Transaction`.

### ~~1.3 Vector Return Variants~~ DONE

Implemented as `fetchAllVec` in `Valiant.Execute`.

### ~~1.4 forEach (Streaming Callback)~~ DONE

Implemented as `forEach` in `Valiant.Execute`.

### ~~1.5 Batch INSERT ... RETURNING (Pipelined)~~ DONE

Implemented as `executeReturningMany` in `Valiant.Execute`.

### ~~1.6 Connection-Level Transaction~~ DONE

Implemented as `withTransactionConn` and `withTransactionModeConn` in
`Valiant.Transaction`.

---

## Tier 2: Medium Priority

Ergonomics and completeness for production use.

### ~~2.1 hstore Binary Codec~~ DONE

Implemented in `Valiant.Binary.HStore`.

### ~~2.2 macaddr / macaddr8 Binary Codec~~ DONE

Implemented in `Valiant.Binary.MacAddr`.

### ~~2.3 Timestamp Infinity Support~~ DONE

Implemented in `Valiant.Binary.Unbounded`.

### ~~2.4 Convenience Query Functions~~ DONE

Implemented as `fetchOneOr`, `fetchOneOrThrow`, `fetchFirst` in
`Valiant.Execute`.

### ~~2.5 TransactionMode Record and Deferrable Transactions~~ DONE

Implemented as `TransactionMode` record with `withTransactionMode` and
`withTransactionModeConn` in `Valiant.Transaction`.

### ~~2.6 Advisory Lock Helpers~~ DONE

Implemented in `Valiant.Advisory` with session-scoped, transaction-scoped,
and try variants. Soundness-audited (#5, #19).

### ~~2.7 Pool from Connection String~~ DONE

Implemented as `newPoolFromString` in `Valiant`.

### ~~2.8 SQLSTATE Accessor on Errors~~ DONE

Implemented as `sqlState` in `Valiant.Error`.

---

## Tier 3: Low Priority

Nice to have, uncommon use cases.

### ~~3.1 Large Object Support~~ DONE

Implemented in `Valiant.LargeObject` with `withLargeObject` bracket,
import/export, streaming. Soundness-audited (#6, #9, #20, #27).

### 3.2 Geometric Type Codecs

`point` (OID 600) is implemented in `Valiant.Binary.Point`. Remaining
types (`line`, `box`, `path`, `polygon`, `circle`) are uncommon outside
GIS applications, and PostGIS users typically use extension types.

### ~~3.3 ZonedTime for timestamptz~~ DONE

Implemented in `Valiant.Binary.Encode`/`Decode`.

### ~~3.4 timetz Type~~ DONE

Implemented as `(TimeOfDay, TimeZone)` in `Valiant.Binary.Encode`/`Decode`.

### 3.5 Dynamic Statement Builder

For cases like dynamic filtering, optional WHERE clauses, dynamic
ORDER BY. hasql-dynamic-statements provides a `Snippet` type for this.
Less relevant for valiant since users can write multiple `.sql` files,
but some use cases genuinely need dynamic SQL composition.

### 3.6 money Type

PG `money` type (OID 790). Uncommon; the PostgreSQL docs recommend
using `numeric` instead. Users can `CAST(amount AS numeric)` in SQL.

### ~~3.7 Conduit / Streaming Library Integration~~ DONE

8 adapter packages: valiant-conduit, valiant-pipes, valiant-streaming,
valiant-streamly, valiant-bluefin, valiant-effectful, valiant-fused-effects,
valiant-mtl.

### ~~3.8 Tuples Beyond 10~~ DONE

`FromRow` and `ToParams` instances extend to 16-tuples
(`runtime/src/Valiant/FromRow.hs`, `runtime/src/Valiant/ToParams.hs`),
with round-trip coverage in `runtime/test/Valiant/TupleSpec.hs`.
postgresql-simple goes to 20; 16 covers the realistic-schema envelope.

### 3.9 refine Decoder Combinator

hasql has `refine :: (a -> Either Text b) -> Value a -> Value b` for
post-decode validation. Less relevant for us since decoders are
plugin-generated, but could be useful for custom types.

---

## Existing Strengths (No Gap)

Features where valiant matches or exceeds all competitors:

| Feature | Notes |
|---------|-------|
| Compile-time SQL validation | Unique to valiant (via GHC plugin) |
| Binary protocol | hasql uses libpq binary; pg-simple uses text; we use our own binary |
| Pipelined batch writes | 40-100x faster than competitors |
| Pipeline Applicative | Combines independent reads in one round-trip |
| Server-side cursors | Built-in `withCursor` + `fetchBatch`; `fetchAllCursor` for drain-into-list |
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
| No C dependencies | Pure Haskell (no libpq, no system deps) |
| Structured error messages | 9 error types with column-by-column diagnostics |
| hstore codec | `Valiant.Binary.HStore` |
| macaddr / macaddr8 codec | `Valiant.Binary.MacAddr` |
| Timestamp infinity | `Valiant.Binary.Unbounded` |
| Advisory locks | Session-scoped, transaction-scoped, try variants |
| Large objects | `withLargeObject` bracket, import/export, streaming |
| Streaming adapters | 8 packages: conduit, pipes, streaming, streamly, bluefin, effectful, fused-effects, mtl |

---

## Comparison Matrix

| Feature | valiant | hasql | pg-simple | Notes |
|---------|:-----:|:-----:|:---------:|-------|
| Compile-time SQL checking | **Yes** | No | No | GHC plugin |
| Binary wire protocol | **Yes** | Yes (libpq) | No (text) | |
| Pipelined batching | **Yes** | Yes (Pipeline) | No | |
| Connection pool | **Built-in** | Separate pkg | No | |
| Transactions | **Built-in** | Separate pkg | Built-in | |
| Transaction retry | **Yes** | Auto (serializable) | Yes | |
| Constraint violation helpers | **Yes** | No | Yes | |
| Vector result return | **Yes** | Yes (default) | Yes (module) | |
| forEach callback | **Yes** | No | Yes | |
| Batch RETURNING | **Yes** | No | Yes (`returning`) | |
| Connection-level txn | **Yes** | Yes | Yes | |
| hstore codec | **Yes** | No | Yes | |
| macaddr codec | **Yes** | No | No | |
| Timestamp infinity | **Yes** | No | Yes | |
| Large objects | **Yes** | No | Yes | |
| inet/cidr | **Yes** | Yes (iproute) | No | |
| Generic row derivation | **Yes** | No (manual) | No (manual) | |
| Streaming cursors | **Yes** | Manual only | Yes (fold) | |
| COPY protocol | **Yes** | No | Yes | |
| LISTEN/NOTIFY | **Yes** | Separate pkg | Partial | |
| Constant-memory fold | **Yes** | Yes (foldlRows) | Yes (fold) | |
| No system C deps | **Yes** | No (libpq) | No (libpq) | |

# Soundness & Correctness Audit

Conducted 2026-03-23. Covers `pg-wire` and `hsqlx` runtime internals.

**Status: ALL FINDINGS RESOLVED** (2026-03-23)

- 19 issues fixed across 8 commits
- 2 false positives (#18 pool psInUse, #24 BEGIN masking)
- 3 LOW items addressed via documentation

Each finding includes a performance impact assessment. Fixes that add overhead
to hot paths are flagged so we can make explicit tradeoff decisions.

---

## CRITICAL — Silent data corruption or connection death

| # | Issue | Location | Performance impact |
|---|-------|----------|--------------------|
| 1 | Fast-path send lock race | `wire/src/PgWire/Async.hs:255-266` | None — reordering two existing operations |
| 2 | CloseComplete error recovery skips ReadyForQuery | `wire/src/PgWire/Async.hs:552-558` | None — only fires on error path |
| 3 | Cursor not closed on exception | `runtime/src/Hsqlx/Streaming.hs:72-99` | None — `bracket` adds no measurable overhead |
| 4 | COPY IN producer exception hangs connection | `runtime/src/Hsqlx/Copy.hs:45-51` | None — error path only |
| 5 | Advisory lock leak in try variant | `runtime/src/Hsqlx/Advisory.hs:60-68` | None — `bracket` on lock release |
| 6 | LargeObject path injection | `runtime/src/Hsqlx/LargeObject.hs:145-159` | Negligible — one `escapeLiteral` call per import/export |

### 1. Fast-path send lock race

The send lock (`awcSendLock`) is released *before* the pending response is
guaranteed to be ordered in the reader queue. Under contention, thread B can
acquire the lock, send its messages, and enqueue its `PendingResponse` before
thread A's enqueue completes — breaking the FIFO invariant between sent
messages and pending responses. The reader then delivers responses to the
wrong callers.

**Fix:** Move `putTMVar awcSendLock ()` into the same `atomically` block as
the `writeTQueue awcPending` call, or release the lock only after enqueue.

### 2. CloseComplete error recovery skips ReadyForQuery

`waitCloseCompleteLoop` handles `ErrorResponse` by returning immediately.
Per the PostgreSQL protocol, an error during `Close` (sent with `Sync`) is
followed by `ReadyForQuery`. The orphaned `ReadyForQuery` is consumed by the
next request's reader, corrupting response matching.

**Fix:** After `ErrorResponse`, call `drainUntilReady` to consume the
subsequent `ReadyForQuery` and update transaction status.

### 3. Cursor not closed on exception

`withCursor` runs the user action without `bracket` or `finally`. If the
action throws, the `CLOSE` cursor command is never sent. The server-side
cursor leaks, potentially holding locks and consuming resources until the
transaction ends.

**Fix:** Wrap in `bracket` so `CLOSE` is sent in the cleanup action.

### 4. COPY IN producer exception hangs connection

If the producer callback in `copyIn` throws, neither `CopyDone` nor
`CopyFail` is sent. The server remains in COPY mode waiting for more data.
The connection is permanently stuck — all subsequent operations fail or block.

**Fix:** Catch exceptions from the producer and send `CopyFail` with the
error message before re-throwing.

### 5. Advisory lock leak in try variant

`withAdvisoryLockTry` only releases the lock in the success path. If the
user action throws, `advisoryUnlock` is never called. The advisory lock
leaks for the remainder of the session.

**Fix:** Use `bracket` or `finally` to guarantee `advisoryUnlock` runs.

### 6. LargeObject path injection

`loImport` and `loExport` concatenate file paths directly into SQL strings
without escaping. A path containing a single quote enables SQL injection:
`loImport conn "'; DROP TABLE users; --"`.

**Fix:** Use `escapeLiteral` on the path before interpolation.

---

## HIGH — Connection left in bad state or wrong results

| # | Issue | Location | Performance impact |
|---|-------|----------|--------------------|
| 7 | Exclusive mode deadlock on async exception | `wire/src/PgWire/Async.hs:364-372` | **Tradeoff** — timeout adds a `race` call per exclusive op |
| 8 | Savepoint cleanup incomplete | `runtime/src/Hsqlx/Transaction.hs:260-271` | None — removes unnecessary RELEASE |
| 9 | LargeObject FD leak on exception | `runtime/src/Hsqlx/LargeObject.hs:88-92` | None — `bracket` on open/close |
| 10 | Batch.hs missing eviction | `runtime/src/Hsqlx/Batch.hs:109-126` | Negligible — one `PSQ.size` check per miss |
| 11 | Pipeline pipeExecute returns wrong row count | `runtime/src/Hsqlx/Pipeline.hs:94-100` | **Tradeoff** — tracking per-command tags adds allocation |

### 7. Exclusive mode deadlock on async exception

If the caller holding exclusive mode is killed (`cancel`, timeout, async
exception), the writer thread blocks forever on `takeMVar doneVar`. The
connection becomes permanently stuck — no further requests can be processed.

**Fix:** Use `race` with a timeout, or make the done signal interruptible.

**Performance note:** Exclusive mode is used for streaming batches, COPY,
and fold operations — relatively infrequent compared to normal queries. A
`race` call per exclusive op is likely acceptable, but worth benchmarking
if exclusive-mode throughput is important.

### 8. Savepoint cleanup incomplete

The exception handler in `withSavepoint` issues `ROLLBACK TO SAVEPOINT`
followed by `RELEASE SAVEPOINT`. If the rollback succeeds but the release
fails, the savepoint is in a half-cleaned state. More fundamentally, after
`ROLLBACK TO SAVEPOINT`, the savepoint still exists and doesn't need
explicit release — PostgreSQL automatically cleans it up at transaction end.

**Fix:** Remove the `RELEASE SAVEPOINT` from the exception handler. Only
issue `ROLLBACK TO SAVEPOINT` on error; `RELEASE SAVEPOINT` on success.

### 9. LargeObject FD leak on exception

No `bracket` around `loOpen`/`loClose`. If operations between them throw,
the server-side file descriptor leaks until the enclosing transaction ends.

**Fix:** Provide a `withLargeObject` bracket function.

### 10. Batch.hs missing eviction

`ensurePreparedRaw` in `Batch.hs` doesn't call `evictIfNeeded` before
allocating a new statement name, unlike every code path in `Execute.hs`.
The cache can exceed 256 entries via this path.

**Fix:** Add `evictIfNeeded conn cache` before allocation, matching
`Execute.hs` behavior.

### 11. Pipeline pipeExecute returns wrong row count

`pipeExecute` always returns 0 for affected rows. The server collects
command tags, but they're silently discarded by the response collector.
Users get incorrect results with no indication.

**Fix:** Track per-command tags in the pipeline collector.

**Performance note:** This requires collecting and summing `CommandTag`
values in the pipeline response path. For large pipelines (hundreds of
commands), this adds allocation for each tag. The overhead is likely small
relative to the I/O cost but should be measured if pipeline throughput is
a priority.

---

## MEDIUM — Robustness gaps, edge-case failures

| # | Issue | Location | Performance impact |
|---|-------|----------|--------------------|
| 12 | Negative payload length → memory exhaustion | `wire/src/PgWire/Wire.hs:295-307` | None — one comparison per message |
| 13 | SCRAM server signature not constant-time | `wire/src/PgWire/Auth/ScramSHA256.hs:105-108` | None — one-time during auth |
| 14 | Notification handler blocks reader thread | `wire/src/PgWire/Async.hs:603-614` | **Tradeoff** — async dispatch adds thread spawn per notification |
| 15 | Statement cache desync after DISCARD ALL | `runtime/src/Hsqlx/Execute.hs:625-642` | **Tradeoff** — cache clear on recycle adds IORef write per checkout |
| 16 | Health check blocks acquire indefinitely | `wire/src/PgWire/Pool.hs:468-489` | **Tradeoff** — socket timeout adds syscall per health check |
| 17 | Reaper/warmer threads not restarted on crash | `wire/src/PgWire/Pool.hs:164-674` | None — catch at top of loop |
| 18 | Pool `psInUse` can go negative | `wire/src/PgWire/Pool.hs:304, 399-420` | None — one extra TVar write in STM transaction |
| 19 | Transaction-scoped advisory lock not verified | `runtime/src/Hsqlx/Advisory.hs:100-103` | **Tradeoff** — TxStatus IORef read per lock call |
| 20 | LargeObject silent parse failures | `runtime/src/Hsqlx/LargeObject.hs:95-134` | None — replace silent fallback with error |
| 21 | FromRow ignores extra columns | `runtime/src/Hsqlx/FromRow.hs` | **Tradeoff** — column count check adds one comparison per row decode |

### 12. Negative payload length → memory exhaustion

If a malformed or malicious message has `len < 4`, `payloadLen` goes
negative. `fromIntegral` wraps it to a huge positive `Word`, and `wcRecv`
attempts to allocate gigabytes.

**Fix:** Guard `payloadLen < 0` and throw `ProtocolError`.

### 13. SCRAM server signature not constant-time

Server signature comparison uses `(/=)`, which short-circuits on the first
differing byte. A timing oracle could theoretically enable server
impersonation by brute-forcing the signature byte-by-byte.

**Fix:** Use `Data.ByteArray.constEq` from the `memory` package (already
a dependency).

### 14. Notification handler blocks reader thread

Notification and notice handlers run synchronously in the reader thread.
A blocking handler (e.g., one that tries to acquire a lock held by a thread
waiting for a query response) deadlocks all pending requests on the
connection.

**Fix options:**
- (a) Dispatch to a separate thread via `forkIO` — adds one thread spawn
  per notification but guarantees reader progress.
- (b) Document that handlers must be non-blocking and add a timeout wrapper.
- (c) Queue notifications into a `TBQueue` and let users drain at their pace.

**Performance note:** Option (a) is safest but adds overhead for
notification-heavy workloads (LISTEN on a busy channel). Option (c) is
zero-cost on the reader thread but changes the API. Option (b) is
zero-overhead but relies on user discipline.

### 15. Statement cache desync after DISCARD ALL

The prepared statement cache has no invalidation mechanism. If `DISCARD ALL`
runs — either via `RecycleClean` recycling, an explicit user query, or a
proxy/pgBouncer injecting it — cached statement names point to deallocated
server-side statements. The next cache hit sends a `Bind` referencing a
nonexistent statement, producing "prepared statement does not exist."

**Fix options:**
- (a) Clear the cache when `RecycleClean` is used (the pool knows it ran
  `DISCARD ALL`). Zero cost on the normal path.
- (b) Intercept `DISCARD` in `simpleQuery` and clear the cache. Adds a
  bytestring prefix check per simple query.
- (c) On "prepared statement does not exist" error, evict the entry and
  retry transparently. Adds a catch per query execution.

**Performance note:** Option (a) is sufficient for the pool-managed case
and costs nothing. Option (c) is the most robust but adds overhead to every
query for a rare error. Recommend (a) initially, with (c) as a follow-up
if users hit the external-DISCARD case.

### 16. Health check blocks acquire indefinitely

`RecycleVerified` and `RecycleClean` send a query during connection
checkout. If the backend has hung (TCP connection alive but process stuck),
`simpleQuery` blocks until TCP timeout (typically minutes). The caller's
`withResource` blocks for the entire duration with no way to bail out.

**Fix:** Add a socket-level or application-level timeout to health check
queries. The pool already has `poolAcquireTimeout` — the health check
should respect it.

**Performance note:** Adding a `race` with `threadDelay` per health check
is cheap but not free. Since health checks only fire when a connection has
been idle longer than `poolHealthCheckAge` (default 5s), this is infrequent
on active pools.

### 17. Reaper/warmer threads not restarted on crash

The reaper and warmer run as bare `async` threads with no exception handler.
If any operation in the loop throws (e.g., `getCurrentTime` on a system
clock error, or `destroyEntry` hitting a weird state), the thread dies
silently. Idle connections are never reaped thereafter.

**Fix:** Wrap the loop body in `catch` with a delay-and-retry, or use
`forkFinally` with a restart callback.

### 18. Pool `psInUse` can go negative

`pActive` is only incremented when creating new connections (`CreateNew`
path), not when reusing idle ones (`GotIdle` path). Since
`psInUse = active - idle`, recycling idle connections produces negative
in-use counts in `poolStats`.

**Fix:** Increment `pActive` in the `GotIdle` branch of `acquireAction`
(same STM transaction that removes from idle queue). This makes `pActive`
mean "total allocated connections" rather than "creation slots in flight."

### 19. Transaction-scoped advisory lock not verified

`withAdvisoryLockTx` doesn't verify that a transaction is active. If called
outside `withTransaction`, PostgreSQL acquires a session-scoped lock instead
of transaction-scoped, contradicting the function's semantics.

**Fix:** Read `connTxStatus` and throw if not in a transaction.

**Performance note:** One `readIORef` per lock call — negligible.

### 20. LargeObject silent parse failures

`loRead` returns empty bytes on parse error. `loTell` and `loSeek` return 0.
Callers cannot distinguish between valid empty results and errors.

**Fix:** Return parse errors via `Either` or throw `ProtocolError`.

### 21. FromRow ignores extra columns

If a query returns more columns than the Haskell type expects, the extras
are silently dropped. This masks schema evolution bugs — a column added to
a table won't cause a compilation or runtime error, and the data is quietly
lost.

**Fix options:**
- (a) Validate column count matches expected field count. Adds one integer
  comparison per row decode.
- (b) Provide a `FromRowStrict` class that validates, alongside the current
  lenient `FromRow`. Zero cost for users who don't opt in.

**Performance note:** Option (a) adds a trivial branch to the hot decode
path. For bulk reads (millions of rows), even a branch can matter if it
defeats prediction. Option (b) avoids this entirely. Recommend (b) unless
benchmarks show (a) is free.

---

## LOW — Correctness notes, documentation gaps

| # | Issue | Location | Performance impact |
|---|-------|----------|--------------------|
| 22 | Array codec rejects NULL elements | `runtime/src/Hsqlx/Binary/Array.hs:72` | N/A — design decision |
| 23 | TypeCache grows unbounded | `wire/src/PgWire/TypeCache.hs` | N/A — bounded by type count in practice |
| 24 | `BEGIN` not wrapped in `mask` | `runtime/src/Hsqlx/Transaction.hs:95-100` | Negligible — extends existing `mask` scope |

### 22. Array codec rejects NULL elements

PostgreSQL arrays can contain NULL elements, but the `Vector`-based decoder
rejects them with "NULL elements not supported in Vector decode." This is a
known limitation — `Vector a` cannot represent NULLs without wrapping in
`Maybe`. Users who need nullable array elements must use a different type.

**Action:** Document the limitation. Consider adding a `Vector (Maybe a)`
decode path.

### 23. TypeCache grows unbounded

The pool-level `TypeCache` maps OIDs to type metadata with no eviction.
In practice this is bounded by the number of distinct custom types in the
database (typically tens, not thousands). Unbounded growth is only a concern
for applications that dynamically create and drop types.

**Action:** Document the assumption. Optionally add a size cap with LRU
eviction if dynamic-type workloads are a target.

### 24. BEGIN not wrapped in mask

In `withTransaction`, `simpleQuery conn "BEGIN"` runs before the `mask`
block. An async exception between `BEGIN` succeeding and `mask` taking
effect leaves the connection in a transaction with no cleanup. The
connection is returned to the pool in transaction state.

**Fix:** Move `BEGIN` inside the `mask` block, or use `mask_` around the
entire sequence.

---

## Performance tradeoff summary

These fixes touch hot paths and need benchmarking before/after:

| # | Fix | Hot path? | Expected overhead | Decision needed? |
|---|-----|-----------|-------------------|------------------|
| 7 | Exclusive mode timeout | Streaming, COPY, fold | One `race` per op | Probably acceptable — measure |
| 11 | Pipeline row count tracking | Pipeline execution | Tag allocation per command | Measure for large pipelines |
| 14 | Async notification dispatch | Reader thread | Thread spawn per notification | Choose strategy (a/b/c) |
| 15 | Cache invalidation on DISCARD | Pool checkout | IORef write per RecycleClean checkout | Option (a) is zero-cost on normal path |
| 16 | Health check timeout | Pool checkout | `race` per health check | Infrequent — likely fine |
| 19 | Transaction status check | Advisory lock | One IORef read | Negligible |
| 21 | Column count validation | Row decode | One comparison per row | Benchmark with bulk reads |

All other fixes are on error paths, initialization, or infrequent operations
and have no measurable performance impact.

# Soundness & Correctness Audit

Conducted 2026-03-23 across three review passes. Covers `pg-wire` and
`valiant` runtime internals.

**Status: ALL FINDINGS RESOLVED** (2026-03-23)

- 3 audit passes, 30+ findings investigated
- 25 code fixes across 12 commits
- 4 false positives confirmed (#18 pool psInUse, #24 BEGIN masking,
  TLS buffer stale data, exclusive mode signalDone race)
- 2 documentation improvements (#22 array NULLs, #23 TypeCache growth)
- 12 integration tests added validating the fixes against real PostgreSQL

Each finding includes a performance impact assessment. All fixes on hot
paths were verified to have zero or negligible overhead.

---

## Pass 1: Initial audit

### CRITICAL: Silent data corruption or connection death

All resolved. Zero performance impact (error paths only).

| # | Issue | Fix | Commit |
|---|-------|-----|--------|
| 1 | **Fast-path send lock race**: lock released before pending enqueued, breaking FIFO | Combined `writeTQueue` + `putTMVar` in single `atomically` block | `b1233f3` |
| 2 | **CloseComplete error recovery**: `ErrorResponse` silently swallowed without draining | Throw `QueryError` so reader's error recovery drains `ReadyForQuery` | `b1233f3` |
| 3 | **Cursor not closed on exception**: server-side cursor leaks on throw | `onException closeCursor` in `withCursor`; cleanup wrapped in `catch` to preserve original exception | `92efcfc`, `aeb7ebb` |
| 4 | **COPY IN producer exception hangs connection**: neither `CopyDone` nor `CopyFail` sent | Catch producer exceptions, send `CopyFail`, `drainToReady` to consume `ErrorResponse` + `ReadyForQuery` | `92efcfc`, `43f0060` |
| 5 | **Advisory lock leak in try variant**: `advisoryUnlock` only in success path | Use `finally` to guarantee unlock | `92efcfc` |
| 6 | **LargeObject path injection**: file paths interpolated into SQL unescaped | Use `escapeLiteral` on paths in `loImport`/`loExport` | `92efcfc` |

### HIGH: Connection left in bad state or wrong results

All resolved.

| # | Issue | Fix | Commit |
|---|-------|-----|--------|
| 7 | **Exclusive mode deadlock on async exception**: writer hangs on `takeMVar doneVar` | Wrap `submitExclusive` in `mask`/`onException`/`restore`; `signalDone` always fires | `b1233f3` |
| 8 | **Savepoint cleanup incomplete**: `RELEASE SAVEPOINT` after `ROLLBACK TO` can fail in aborted state | Remove `RELEASE` from exception handler; only `ROLLBACK TO` on error | `7777a4b` |
| 9 | **LargeObject FD leak on exception**: no bracket around `loOpen`/`loClose` | Added `withLargeObject` bracket function | `7777a4b` |
| 10 | **Batch.hs missing eviction**: `ensurePreparedRaw` skips `evictIfNeeded` | Added eviction call matching `Execute.hs` | `92efcfc` |
| 11 | **Pipeline `pipeExecute` returns wrong row count**: always returns 0 | Changed return type from `Int64` to `()` (breaking API change); row counts unavailable in pipeline mode by design | `7777a4b` |

### MEDIUM: Robustness gaps, edge-case failures

All resolved except #18 (false positive).

| # | Issue | Fix | Commit |
|---|-------|-----|--------|
| 12 | **Negative payload length → memory exhaustion** | Guard `payloadLen < 0`, throw `ProtocolError` | `0fdd7b0` |
| 13 | **SCRAM signature not constant-time** | Use `BA.constEq` from `memory` package | `0fdd7b0` |
| 14 | **Notification handler blocks reader thread** | Dispatch handlers via `forkIO` (~1μs overhead per notification) | `645731d` |
| 15 | **Statement cache desync after DISCARD ALL** | Clear `connStmtCache` after successful `RecycleClean` | `9cd65a9` |
| 16 | **Health check blocks acquire indefinitely** | Add `race` timeout (uses `poolAcquireTimeout`) to `RecycleVerified` and `RecycleClean` health checks | `9cd65a9` |
| 17 | **Reaper/warmer threads die on exception** | Wrap loop body in `catch`; re-throw `AsyncException` for clean shutdown, catch synchronous exceptions and continue | `0fdd7b0`, `aeb7ebb` |
| 18 | **Pool `psInUse` can go negative** | **False positive.** `pActive` correctly tracks total living connections; `psInUse = active - idle` is accurate. | N/A |
| 19 | **Transaction-scoped advisory lock not verified** | `requireTransaction` checks `TxStatus` IORef before acquiring | `0fdd7b0` |
| 20 | **LargeObject silent parse failures** | Replace `pure 0`/`pure empty` with `throwValiant (DecodeError ...)` | `0fdd7b0`, `62cd234` |
| 21 | **FromRow ignores extra columns** | Added opt-in `FromRowStrict` class with column count validation; zero cost for `FromRow` users | `9cd65a9` |

### LOW: Documentation

| # | Issue | Resolution | Commit |
|---|-------|------------|--------|
| 22 | Array codec rejects NULL elements | Documented in `pgDecodeArray` haddock | `532e34d` |
| 23 | TypeCache grows unbounded | Documented in module header; bounded in practice by distinct type count | `532e34d` |
| 24 | `BEGIN` not wrapped in `mask` | **False positive.** `BEGIN` is already inside `mask` in all transaction functions. | N/A |

---

## Pass 2: Review of fixes + deeper analysis

| # | Issue | Fix | Commit |
|---|-------|-----|--------|
| 25 | **Fast-path lock stuck on `sendFrontendMsgs` exception**: lock never released, future fast-path calls degrade to slow queue | `onException releaseLock` around `sendFrontendMsgs` | `62cd234` |
| 26 | **Transaction COMMIT failure poisons pool**: deferred constraint violation leaves connection in aborted state, returned to pool | `onException rollback conn` on COMMIT in all 3 transaction functions | `62cd234` |
| 27 | **LargeObject `fail` throws `IOException` not `ValiantError`**: breaks library error contract | Replace all `fail` with `throwValiant (DecodeError ...)` | `62cd234` |

**False positives investigated:**
- Exclusive mode `signalDone` internal race: safe, runs inside `mask`, both ops are pure MVar operations
- Writer batch pending-before-send: handled by `onDeath` + `link2` (reader/writer die together)
- TLS buffer stale data: PostgreSQL sends exactly 1 byte before handshake; no extra data in buffer

**COMMIT failure trade-off (documented in code):** If the network drops after
the server commits but before the client receives the response, the client
will attempt `ROLLBACK` on an already-committed transaction. This is an
inherent TCP limitation. Without two-phase commit, the client cannot
distinguish "committed but response lost" from "failed to commit." The fix
prevents pool poisoning (HIGH severity) at the cost of this edge case.

---

## Pass 3: Exception handling consistency

| # | Issue | Fix | Commit |
|---|-------|-----|--------|
| 28 | **Reaper/warmer catch `ThreadKilled`**: prevents clean pool shutdown | Re-throw `AsyncException`, only catch synchronous exceptions | `aeb7ebb` |
| 29 | **Copy/Streaming cleanup masks original exception**: if wire is dead, cleanup throws and original exception is lost | Wrap cleanup handlers in `catch` so original exception always re-throws | `aeb7ebb` |
| 30 | **`Dynamic.hs` `fail` throws `IOException`**: inconsistent with `ValiantError` | Replace with `throwValiant (DecodeError ...)` | `aeb7ebb` |
| 31 | **`ccMaxPreparedStatements` not validated**: zero/negative silently disables caching | Clamp to `max 1` at connection creation | `aeb7ebb` |

---

## Integration test validation

Running the integration tests against real PostgreSQL caught two additional
bugs not visible in code review:

| # | Issue | Fix | Commit |
|---|-------|-----|--------|
| 32 | **Copy `drainToReady`**: `collectCopyResult` throws on `ErrorResponse` before reaching `ReadyForQuery`, leaving orphaned message | Replace `collectCopyResult` with `drainToReady` in error path | `43f0060` |
| 33 | **Fold exception leaves protocol debris**: exception in fold step leaves `CommandComplete` + `ReadyForQuery` in socket buffer | Catch fold exceptions, `drainUntilReady` before re-throwing | `43f0060` |

**Test coverage (12 tests in `SoundnessSpec.hs`):**
- COMMIT failure recovery (deferred constraint violation)
- Savepoint cleanup (multiple failures in sequence)
- COPY exception safety (producer throws, connection stays usable)
- COPY exception preservation (original exception message retained)
- Fold exception cleanup (step throws, connection stays usable)
- Advisory lock release on exception (session-scoped)
- Advisory try-lock release on exception
- Advisory transaction-scoped lock requires active transaction
- Statement cache eviction (>256 unique queries)
- LRU eviction correctness (hot queries survive)
- RecycleClean cache invalidation (DISCARD ALL clears cache)
- Configurable cache size

---

## Configurable statement cache size

Added `ccMaxPreparedStatements` to `ConnConfig` (default: 256). Parseable
from connection strings as `statement_cache_size`. Clamped to minimum 1.

Higher values reduce re-parses for workloads with many distinct queries.
Lower values are appropriate for PgBouncer or memory-constrained setups.

---

## Performance impact summary

| Category | Fixes | Hot path impact |
|----------|-------|-----------------|
| Error/cleanup paths | #1-6, #8-10, #12, #20, #25-27, #29-30, #32-33 | **Zero** (only fire on exceptions) |
| Protocol correctness | #2, #7 | **Zero** (reordering existing ops) |
| Security | #6, #13 | **Negligible** (one `escapeLiteral` or `constEq` per call) |
| Pool background threads | #17, #28 | **Zero** (catch at top of existing loop) |
| Notification dispatch | #14 | **~1μs per notification** (`forkIO` green thread) |
| Health check timeout | #16 | **One `race` per idle health check** (infrequent on active pools) |
| Cache invalidation | #15 | **One `writeIORef` per `RecycleClean` checkout** (zero on normal path) |
| Advisory lock check | #19 | **One `readIORef` per lock call** |
| API changes | #11 (`pipeExecute` → `()`), #21 (`FromRowStrict`) | **Zero** (additive or type-only) |

All fixes verified with: 80 integration tests, 234 pg-wire unit tests,
264 valiant unit tests, all passing.

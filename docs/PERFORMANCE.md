# Performance

hsqlx implements its own PostgreSQL wire protocol in pure Haskell with binary
format encoding. This document covers the benchmark results, the techniques
that make it fast, and the optimization journey.

## Benchmark results

All benchmarks run on the same machine against Docker Postgres 16 on localhost.
Single connection, no connection pool overhead. Compared against
[hasql](https://hackage.haskell.org/package/hasql) (libpq FFI, binary format),
[postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)
(libpq FFI, text format), and
[persistent](https://hackage.haskell.org/package/persistent)
(ORM built on postgresql-simple).

### Read performance

| Rows | hsqlx | hasql | pg-simple | persistent | vs hasql | vs persistent |
|------|-------|-------|-----------|------------|----------|---------------|
| 1 (by PK) | **0.94 ms** | 1.0 ms | 1.0 ms | 3.2 ms | **6% faster** | **3.4x faster** |
| 1,000 | **4.7 ms** | 7.7 ms | 8.7 ms | 11.7 ms | **39% faster** | **2.5x faster** |
| 5,000 | **20.8 ms** | 38.0 ms | 42.5 ms | 47.4 ms | **45% faster** | **2.3x faster** |
| 10,000 | **37.2 ms** | 72.3 ms | 83.0 ms | 94.1 ms | **48% faster** | **2.5x faster** |

hsqlx is the fastest Haskell PostgreSQL library for reads. The advantage
grows with row count because the per-row decode overhead is lower.
persistent adds ~2x overhead on single-row operations and ~10-15% over
pg-simple on multi-row reads due to its monad transformer stack.

### Insert performance

| Rows | hsqlx (pipelined) | hsqlx (seq) | hasql | pg-simple | persistent |
|------|-------------------|-------------|-------|-----------|------------|
| 100 | **2.5 ms** | 104 ms | 111 ms | 118 ms | 106 ms |
| 1,000 | **13.0 ms** | 1.14 s | 1.15 s | 1.12 s | ~1.1 s |
| 5,000 | **53.5 ms** | 5.26 s | 5.16 s | 5.91 s | ~5.5 s |

Pipelined batch inserts (`executeBatch`) are **40-100x faster** than
sequential inserts with any library. Sequential inserts are equivalent
across all libraries (dominated by per-row round-trip time).

### Update performance (single UPDATE affecting N rows)

| Rows | hsqlx | hasql | pg-simple | persistent |
|------|-------|-------|-----------|------------|
| 100 | **1.5 ms** | 1.5 ms | 1.5 ms | 3.2 ms |
| 1,000 | **4.7 ms** | 4.6 ms | 4.8 ms | ~6 ms |
| 5,000 | **18.8 ms** | 17.7 ms | 17.5 ms | ~20 ms |

Single-statement updates are dominated by Postgres server-side execution.
All raw drivers perform similarly. persistent adds ~2x overhead on small
updates due to its per-query monad stack cost.

### Codec performance (per-value)

| Type | Encode | Decode |
|------|--------|--------|
| Bool | 23 ns | 20 ns |
| Int16 | 36 ns | 25 ns |
| Int32 | 30 ns | 25 ns |
| Int64 | 28 ns | 19 ns |
| Float | 27 ns | 30 ns |
| Double | 31 ns | 26 ns |
| Text (5 chars) | 32 ns | 53 ns |
| Day | 46 ns | 28 ns |
| UTCTime | 300 ns | 58 ns |
| Scientific | 596 ns | 102 ns |
| UUID | — | — |
| Int32 array (1000 elems) | 66 μs | 46 μs |

### Pool performance

| Benchmark | Time |
|-----------|------|
| Acquire/release (pool-size-1) | 27.0 ms |
| Acquire/release (pool-size-4) | 28.8 ms |
| Acquire/release (pool-size-16) | 33.1 ms |

Acquire/release overhead includes connection creation on cold start.
Once warm, pool acquire is sub-millisecond (see "pool acquire/release"
in single-connection benchmarks: 979 μs).

**Contention scaling (32 threads × 10 queries each):**

| Pool Size | Time | Notes |
|-----------|------|-------|
| 4 | 164 ms | Threads wait for connections |
| 8 | 116 ms | Less contention |
| 16 | 136 ms | More connections, more context switching |
| 32 | 186 ms | 1 connection per thread, no sharing benefit |

Sweet spot is pool-size = 8 for 32 threads. Smaller pools force waiting;
larger pools have diminishing returns from connection overhead.

**Recycling methods (10 acquire/release cycles, pool-size-4):**

| Method | Time | Description |
|--------|------|-------------|
| RecycleFast | 34.9 ms | Check alive TVar only (no I/O) |
| RecycleVerified | 45.9 ms | Empty query if idle > threshold |
| RecycleClean | 46.7 ms | DISCARD ALL before reuse |

RecycleFast is ~25% faster. RecycleVerified and RecycleClean are
equivalent because the health-check query and DISCARD ALL have
similar round-trip cost.

### Libraries not benchmarked

We considered benchmarking additional libraries but concluded the results
would not provide new information:

**rel8** ([hackage](https://hackage.haskell.org/package/rel8)) is a
type-safe query builder that generates SQL from a Haskell DSL. It builds
entirely on hasql — every rel8 query goes through `Hasql.Connection.use`
→ `Hasql.Session` → libpq. The only thing rel8 adds is Haskell-side
query construction (building the SQL string). Its execution performance
is therefore hasql's numbers plus a few microseconds of DSL evaluation.
Since we already benchmark hasql directly, rel8 can only be equal or
slower at runtime.

**postgresql-typed** ([hackage](https://hackage.haskell.org/package/postgresql-typed))
is the closest conceptual competitor to hsqlx — it validates SQL at
compile time using Template Haskell. However, at runtime it uses
`postgresql-libpq` (the same C FFI as hasql and postgresql-simple),
so its execution numbers would be roughly equal to those libraries.
It also requires a live PostgreSQL connection at compile time (the TH
splices connect to the database during compilation), which makes
benchmark integration impractical. The interesting comparison with
postgresql-typed is architectural, not performance:

| | hsqlx | postgresql-typed |
|---|---|---|
| Compile-time mechanism | GHC source plugin | Template Haskell |
| DB at compile time | No (separate `hsqlx prepare` step) | Yes (TH connects during compilation) |
| Offline builds | Yes (`.hsqlx/` cache) | No |
| Runtime driver | Pure Haskell (pg-wire) | libpq FFI |
| CI friendly | Yes (`hsqlx check`, no DB needed) | Requires DB in CI build |

**esqueleto** ([hackage](https://hackage.haskell.org/package/esqueleto))
is a type-safe SQL DSL built on persistent. It uses the same
`persistent` `SqlBackend` and `runSqlPool` execution path — esqueleto's
overhead is in query construction, not execution. The persistent
benchmarks already capture the runtime cost.

The fundamental reason these libraries cannot match hsqlx on reads is
that they all use libpq (C FFI) for the wire protocol, while hsqlx
implements the protocol in pure Haskell with binary format decoding
directly from the network buffer. No library built on libpq can avoid
the FFI marshaling overhead that hsqlx eliminates.

---

## Why hsqlx is fast

### 1. Binary format decoding

PostgreSQL supports two result formats: text (human-readable) and binary
(native machine representation). `postgresql-simple` uses text format,
requiring string parsing for every value — `"12345"` must be parsed into
an integer. `hasql` uses binary format through `libpq`, but pays FFI
marshaling costs moving data between C and Haskell heap.

hsqlx decodes binary format directly from the network buffer in pure
Haskell. No FFI boundary, no intermediate copies. An Int32 decode is
4 bytes of direct indexing:

```haskell
decodeInt32BE bs =
  let !b0 = fromIntegral (BS.index bs 0) :: Int32
      !b1 = fromIntegral (BS.index bs 1) :: Int32
      !b2 = fromIntegral (BS.index bs 2) :: Int32
      !b3 = fromIntegral (BS.index bs 3) :: Int32
   in Right (b0 `shiftL` 24 .|. b1 `shiftL` 16 .|. b2 `shiftL` 8 .|. b3)
```

### 2. Direct byte writes for encoding

Fixed-size type encoders use `Data.ByteString.Internal.unsafeCreate` to
write directly into a pre-allocated buffer. No `Builder`, no
`LazyByteString`, no intermediate structures:

```haskell
int32BE :: Int32 -> ByteString
int32BE n = unsafeCreate 4 $ \p -> pokeInt32BE p 0 n
```

This is 5-6x faster than the `Builder -> toLazyByteString -> toStrict`
pipeline for fixed-size types (30ns vs 177ns for Int32).

### 3. Pipelined execution

The PostgreSQL extended query protocol allows sending multiple
Bind+Execute message pairs before a single Sync. `executeBatch` exploits
this to eliminate N-1 network round-trips for N inserts:

```
Sequential (N round-trips):      Pipelined (1 round-trip):
  Bind → server                    Bind₁ ─┐
  Execute → server                 Exec₁  │
  Sync → server                    Bind₂  │
  ← BindComplete                   Exec₂  ├→ server (single send)
  ← CommandComplete                  ...   │
  ← ReadyForQuery                  BindN  │
  Bind → server                    ExecN  │
  Execute → server                 Sync  ─┘
  ...                              ← BindComplete₁
                                   ← CommandComplete₁
                                   ...
                                   ← ReadyForQuery
```

Neither hasql nor postgresql-simple expose pipelining.

### 4. Message coalescing and fused encoding

When sending Bind+Execute+Sync (or any sequence of messages), hsqlx
fuses all protocol messages into a single `Builder`, materializes once
via `buildFrontendMsgsConcat`, and sends with a single `send()` syscall.
For a typical 3-message batch (~100 bytes), this eliminates 2
intermediate `ByteString` allocations compared to per-message encoding.
Combined with `TCP_NODELAY`, this eliminates the latency penalty of
small messages.

### 5. Pre-computed message sizes

The PostgreSQL wire protocol requires the message length before the
payload. hsqlx computes each message's payload size from its fields
without serializing (e.g., `cstringSize bs = BS.length bs + 1`), then
writes tag + length + payload in a single `Builder` pass. This avoids
the double-copy that would come from materializing the payload just to
measure its length.

### 6. Merged header recv

Each backend message is `[tag: 1 byte] [length: 4 bytes] [payload]`.
hsqlx reads the tag and length together in a single 5-byte recv call,
reducing syscalls from 3 to 2 per message.

### 7. Fused row collection and decoding

`fetchAll` decodes each `DataRow` as it arrives from the wire, avoiding
an intermediate `[Vector (Maybe ByteString)]`. The decode function is
applied inside the collection loop, and results are accumulated via
difference lists (O(1) append, no reverse pass).

### 8. Nullable column dispatch via closed type family

The `DecodeColumn` class uses a closed type family to dispatch between
nullable (`Maybe a → Nothing` for NULL) and non-nullable (`a → error`
for NULL) decoding at the type level. No overlapping instances, no
runtime dictionary lookup for the NULL check.

```haskell
type family Nullable (a :: Type) :: Bool where
  Nullable (Maybe _) = 'True
  Nullable _         = 'False
```

---

## Optimization techniques applied

### Strictness

- `StrictData` extension on all packages (all record fields strict by default)
- `-funbox-strict-fields` (GHC automatically unboxes strict fields)
- `-fspecialise-aggressively` (ensures cross-module specialization of
  type class methods like `pgEncode`/`pgDecode`)
- `{-# UNPACK #-}` on all numeric fields in hot-path records (`FieldInfo`,
  `Connection`, `PgInterval`)
- `{-# INLINE #-}` on all codec functions, protocol builders/parsers,
  wire send/recv, and field encode/decode
- Bang patterns on all loop accumulators (`go !acc`, `go !total !n`)
- `foldl'` everywhere (no lazy `foldl`)
- `modifyIORef'` everywhere (no lazy `modifyIORef`)
- `-with-rtsopts=-K8K` on test suites for fail-fast space leak detection

### Allocation reduction

- Direct byte writes via `unsafeCreate` + `pokeByteOff` for fixed-size
  encodes (eliminated `Builder` intermediate for 2-8 byte values)
- Direct-to-Vector `DataRow` parsing via `V.create` with mutable vector
  (eliminated intermediate `[Maybe ByteString]` list entirely)
- `V.fromListN` with known size for `ToParams` tuple encoding
  (Vector pre-allocates exact buffer)
- Difference lists in row collection (eliminates O(n) `reverse`)
- Pre-computed message sizes (eliminates double-copy in protocol encoding)
- Fused row decoding (eliminates intermediate raw row list)
- `ByteString` operations in Scientific encoder (replaced `String`/`[Char]`)

### Safety

- `-fno-full-laziness` on streaming modules (`Streaming.hs`, `Copy.hs`)
  to prevent GHC's full laziness transformation from creating accidental
  sharing that retains data
- Pool health checking with empty query before connection reuse
- Proper `unsafePerformIO` elimination in pool reaper (replaced with
  `IO`-native `partitionM`)

---

## Architecture: pure Haskell vs libpq FFI

```
┌──────────────────────────────────────────────────┐
│  hasql / postgresql-simple                        │
│                                                   │
│  Haskell code                                     │
│    ↓ FFI call                                     │
│  libpq (C)  ← manages sockets, parsing, buffers  │
│    ↓ syscall                                      │
│  TCP socket                                       │
│    ↓                                              │
│  PostgreSQL                                       │
└──────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────┐
│  hsqlx                                            │
│                                                   │
│  Haskell code                                     │
│    ↓ direct ByteString operations                 │
│  PgWire  ← manages socket, buffer, framing        │
│    ↓ syscall                                      │
│  TCP socket                                       │
│    ↓                                              │
│  PostgreSQL                                       │
└──────────────────────────────────────────────────┘
```

| | libpq (FFI) | hsqlx (pure Haskell) |
|---|---|---|
| Single-row latency | Baseline | **Matching** (0.97ms vs 0.99ms) |
| Multi-row throughput | Baseline | **2x faster** at 10K rows |
| Batch writes | No pipelining | **40-100x faster** with `executeBatch` |
| Binary decoding | C heap → Haskell copy | Direct from buffer |
| Fixed-size encoding | N/A | 30ns per Int32 (5.9x improvement) |
| COPY protocol | Requires libpq support | Native |
| LISTEN/NOTIFY | Requires polling libpq | Native async |
| TLS | Linked against OpenSSL | Pure Haskell (`tls` library) |
| Cross-compilation | Requires C toolchain | Pure Haskell |
| Build simplicity | Needs `libpq-dev` | No system dependencies |

---

## Running benchmarks

```bash
# Start Postgres via docker-compose (tuned for benchmarks: tmpfs, fsync=off)
docker compose up -d --wait
export DATABASE_URL="postgres://hsqlx_test:hsqlx_test@localhost:5433/hsqlx_test"

# Codec benchmarks (pure, no database needed)
cabal bench hsqlx-bench --benchmark-options='--match prefix codec'

# Single-thread query benchmarks
cabal bench hsqlx-bench --benchmark-options='--match prefix query'

# Concurrent benchmarks (the async split showcase — use -N for capabilities)
cabal bench hsqlx-bench --benchmark-options='+RTS -N -RTS --match prefix concurrent'

# Pool benchmarks (contention, recycling methods)
cabal bench hsqlx-bench --benchmark-options='+RTS -N -RTS --match prefix pool'

# All benchmarks
cabal bench hsqlx-bench --benchmark-options='+RTS -N -RTS'

# Comparative benchmarks vs hasql and postgresql-simple
cabal run bench-compare

# Teardown
docker compose down
```

---

## Optimization journey

The performance work was done in multiple passes, each informed by profiling,
auditing, and benchmarking. Here's the complete story.

### Pass 1: Wire-level optimizations

The first pass focused on reducing network overhead — the biggest win
for a database driver.

**TCP_NODELAY.** Nagle's algorithm batches small TCP segments for
efficiency, but introduces latency on individual protocol messages.
Disabling it with `TCP_NODELAY` immediately improved single-row
operations by ~25%.

**Message coalescing.** Instead of calling `send()` once per protocol
message, `sendFrontendMsgs` concatenates multiple messages (Bind,
Execute, Sync) into a single `ByteString` and sends them in one syscall.
For a typical query this reduces 3 `send()` calls to 1.

**Pipelined batch execution.** The PostgreSQL extended query protocol
allows multiple Bind+Execute pairs before a single Sync. `executeBatch`
exploits this to send N inserts in a single network round-trip. This
alone delivered 40-100x speedups on batch writes — the single biggest
improvement in the project.

*Result: single-row latency dropped from ~30% slower than hasql to parity.*

### Pass 2: Strictness audit

A comprehensive strictness audit identified and fixed every lazy
accumulation pattern in the codebase.

**Lazy `foldl` in Scientific codec.** Two uses of `foldl` (not `foldl'`)
in the numeric encoder were building chains of `(* 10 + ...)` and
`(* 10000 + ...)` thunks. Fixed to `foldl'`.

**Missing bang patterns on loop accumulators.** Nine loops across six
files had accumulators without bang patterns:

```haskell
-- Before: acc is a lazy thunk chain
go acc = do
  ...
  DataRow vals -> go (vals : acc)

-- After: acc is forced on each iteration
go !acc = do
  ...
  DataRow vals -> go (vals : acc)
```

The most impactful were `decodeInt64BE` (building `shiftL`/`.|.` thunk
chains across 8 iterations on every Int64/timestamp decode),
`collectBatchResult` (building `(+)` thunks on the row counter), and
`parseErrorFields` (accumulating the PgError record lazily).

**`unsafePerformIO` in pool reaper.** The `isIdle` check used
`unsafePerformIO` to read an `IORef`, which GHC could cache via CSE.
Replaced with an `IO`-native `partitionM` in the reaper thread.

**Missing `{-# UNPACK #-}`** on numeric fields in hot-path records.
`FieldInfo` (6 numeric fields allocated per column per `RowDescription`)
and `Connection` (PID + secret key) were boxed. Adding `UNPACK` lets GHC
store the values directly in the constructor.

**Cabal flags.** Added `-funbox-strict-fields` (auto-unbox all strict
fields, since `StrictData` makes them all strict) and
`-fspecialise-aggressively` (ensure cross-module specialization of
`PgEncode`/`PgDecode` type class methods).

**Stack limit on tests.** Added `-with-rtsopts=-K8K` to test suites.
Any space leak immediately stack-overflows, providing fail-fast leak
detection in CI.

*Result: all tests pass with K8K stack limit — zero hidden space leaks.*

### Pass 3: Allocation reduction

The third pass focused on eliminating unnecessary allocations in the
hottest code paths.

**Direct byte writes.** The fixed-size encoders (`int16BE`, `int32BE`,
`int64BE`, `floatBE`, `doubleBE`) were going through
`Builder → toLazyByteString → toStrict` — three allocations for 4 bytes.
Replaced with `Data.ByteString.Internal.unsafeCreate` + `pokeByteOff`:

```haskell
-- Before: 177 ns per Int32
int32BE = LBS.toStrict . B.toLazyByteString . B.int32BE

-- After: 30 ns per Int32 (5.9x faster)
int32BE n = unsafeCreate 4 $ \p -> pokeInt32BE p 0 n
```

**Unrolled Int64 decode.** The `decodeInt64BE` function used a
tail-recursive loop to read 8 bytes. Replaced with fully unrolled
direct indexing (matching the style already used for `decodeInt32BE`):

```haskell
-- Before: loop with accumulator
let go !acc !i
      | i >= 8 = acc
      | otherwise = go (acc `shiftL` 8 .|. fromIntegral (BS.index bs i)) (i + 1)

-- After: 8 direct reads, no loop overhead
let !b0 = fromIntegral (BS.index bs 0) :: Int64
    -- ... b1 through b7 ...
 in Right (b0 `shiftL` 56 .|. b1 `shiftL` 48 .|. ... .|. b7)
```

**`V.fromListN` for known-size Vectors.** The `DataRow` parser and all
`ToParams` tuple instances were using `V.fromList`, which scans the list
to determine the length before allocating. Since the column count and
tuple arity are known statically, `V.fromListN` pre-allocates the exact
buffer size.

**Fused row collection and decoding.** `fetchAll` previously did two
passes: collect all raw rows into `[Vector (Maybe ByteString)]`, then
`mapM decodeRow`. The new `collectAndDecodeRows` decodes each `DataRow`
as it arrives from the wire, eliminating the intermediate list entirely.

**Difference lists.** The row accumulators in `collectRows` and
`collectAndDecodeRows` used prepend-then-reverse (`val : acc` followed
by `reverse acc`). Replaced with difference lists (`acc . (val :)`
followed by `acc []`), eliminating the O(n) reverse traversal.

**Scientific encoder rewrite.** The numeric encoder used `String`
(`[Char]`) for digit manipulation — linked lists of boxed characters.
Rewritten to use `ByteString` operations throughout (`BS8.pack`,
`BS.replicate`, `BS.foldl'`, `BS.splitAt`).

*Result: Int32 encode 5.9x faster (177ns → 30ns), array encode 4.5x
faster, Scientific encode 1.5x faster.*

### Pass 4: Protocol-level optimizations

The fourth pass targeted the protocol encoding and wire framing layers.

**Pre-computed message sizes.** The `withTag` helper was materializing
the payload `Builder` into a `ByteString` just to call `BS.length`,
then wrapping it back into a `Builder` — copying the payload bytes
twice. Replaced with per-message-type size computation functions:

```haskell
-- Before: double-copy
withTag tag payload =
  let payloadBs = LBS.toStrict (B.toLazyByteString payload)  -- copy 1
      len = fromIntegral (BS.length payloadBs + 4) :: Int32
   in B.char8 tag <> B.int32BE len <> B.byteString payloadBs  -- copy 2

-- After: single-pass
Bind portal stmt pfmts vals rfmts ->
  let !sz = cstringSize portal + cstringSize stmt + ...
   in tag 'B' sz <> cstring portal <> cstring stmt <> ...
```

Each message type computes its payload size from field lengths in O(1)
(`cstringSize bs = BS.length bs + 1`, `formatCodesSize fmts = 2 + n * 2`,
etc.), then writes tag + length + payload in a single `Builder` pass.

**Merged header recv.** Each PostgreSQL backend message has a 1-byte tag
followed by a 4-byte length. These were read in two separate `recv()`
calls. Merged into a single 5-byte read, reducing syscalls from 3 to 2
per message (tag+length, then payload).

**`-fno-full-laziness` on streaming modules.** GHC's full laziness
transformation can float expressions out of lambdas, accidentally
creating sharing that retains data in streaming code. Added the pragma
to `Streaming.hs` and `Copy.hs` as a safety measure.

*Result: every protocol message now encodes in a single pass with zero
intermediate allocations for the payload.*

### Pass 5: Sender/receiver split (async pipelining)

The fifth pass was the largest architectural change: splitting each
connection into dedicated writer and reader green threads. This is
the technique behind asyncpg's 3x advantage over psycopg2 on
concurrent workloads.

**Architecture.** After the startup handshake, each connection spawns
two threads:

```
App thread 1 ──┐                     ┌── Writer thread ── sendMany ──┐
App thread 2 ──┼── TBQueue(64) ──────┤                               ├── socket ── PG
App thread N ──┘                     └── Reader thread ── recvMsg  ──┘
                                              │
                                         TQueue (pending MVars, FIFO)
```

Application threads put `Request`s into a `TBQueue`, then block on an
`MVar` for the response. The writer drains the queue, batches messages
via `sendMany`, and enqueues response MVars into a `TQueue`. The reader
parses backend messages and fills MVars in FIFO order.

PostgreSQL processes messages in strict order — the i-th response always
corresponds to the i-th request. No correlation IDs needed.

**Concurrent throughput (single connection, `SELECT` by PK + `COUNT`):**

| Threads | Total queries | Wall time | Queries/sec | Scaling |
|---------|--------------|-----------|-------------|---------|
| 1 | 100 | 124ms | 806/s | 1.0x |
| 4 | 400 | 212ms | 1,887/s | 2.3x |
| 16 | 1,600 | 420ms | 3,810/s | 4.7x |
| 32 | 3,200 | 553ms | 5,787/s | 7.2x |

32 threads on ONE connection achieve 7.2x the throughput of a single
thread. Without the async split, they would serialize and take 32x as
long.

**Key design decisions:**

- *Startup stays serial* — async threads spawn after authentication.
- *TBQueue capacity 64* — backpressure prevents unbounded memory growth.
- *`link2` for thread death* — if reader or writer dies, both die. All
  pending MVars are filled with `ConnectionDead`.
- *COPY/cursors/folds use exclusive mode* — `submitExclusive` pauses
  the pipeline and gives the caller direct socket access, since these
  operations are streaming state machines that can't be multiplexed.

**Reader error recovery.** Query errors (`ErrorResponse`) are per-request,
not connection-fatal. The reader catches `HsqlxError`, drains to
`ReadyForQuery`, delivers the error to the specific caller's MVar, and
continues serving other requests. Without this fix, any query error
would kill the reader thread and the entire connection.

*Result: 7.2x concurrent throughput scaling on a single connection.*

### Pass 6: Parse+Bind+Execute coalescing

Borrowed directly from asyncpg. Before this optimization, a first-time
query required two round-trips:

```
Round-trip 1: Parse + Sync  →  ParseComplete + ReadyForQuery
Round-trip 2: Bind + Execute + Sync  →  rows + ReadyForQuery
```

After coalescing, the first execution is a single round-trip:

```
Round-trip 1: Parse + Bind + Execute + Sync  →  ParseComplete + rows + ReadyForQuery
```

Subsequent executions (cache hit) were always 1 round-trip and are
unchanged.

The reader's row and command collectors skip `ParseComplete` the same
way they skip `BindComplete` — no new collector types needed.

**Flush instead of Sync for preparation.** The `ensurePrepared` path
(used by Pipeline and Fold) now sends `Parse + Flush` instead of
`Parse + Sync`. `Flush` makes Postgres send `ParseComplete` without
the `ReadyForQuery` overhead — one fewer message per cold statement
preparation.

*Result: first-execution latency halved (2 round-trips → 1). Matters
at application startup, new pool connections, and dynamic queries.*

### Pass 7: Allocation and batch streaming

**Constant format vectors.** Every query allocated `V.singleton
BinaryFormat` for parameter and result format codes — a fresh heap
allocation per call for a value that never changes. Replaced with a
module-level `{-# NOINLINE #-}` CAF:

```haskell
binaryFmtVec :: Vector FormatCode
binaryFmtVec = V.singleton BinaryFormat
{-# NOINLINE binaryFmtVec #-}
```

**Streaming batch execution.** `executeBatch` previously materialized
the entire `[FrontendMsg]` message list before sending. For a 10,000-row
insert, that's 20,000 `FrontendMsg` ADT values in memory. Now, batches
larger than 256 items switch to streaming mode: the connection enters
exclusive mode and streams Bind+Execute pairs in chunks of 256, sending
each chunk immediately. Memory is bounded to ~256 messages regardless of
batch size, and Postgres starts processing early rows while the client
is still encoding later ones.

```haskell
-- Small batch (≤256): coalesced through async channel
executeBatch conn stmt small = submitRequest ... (ReqExtendedQuery allMsgs ...)

-- Large batch (>256): streamed in chunks via exclusive mode
executeBatch conn stmt large = submitExclusive ... $ \wc _ -> do
  streamBatchChunks wc name stmt large  -- sends in 256-item chunks
  sendFrontendMsg wc Sync
  collectBatchCmdWire wc ...
```

*Result: bounded memory for large batch inserts, reduced GC pressure
on every query via constant vectors.*

### Pass 8: Fused encoding and direct-to-vector parsing

The eighth pass eliminated the remaining per-message and per-row
allocation overhead in the two hottest paths: protocol encoding and
DataRow parsing.

**Fused Builder encoding.** `sendFrontendMsgs` previously called
`buildFrontendMsg` per message — each doing `toLazyByteString` +
`toStrict` independently — then passed the chunks to `sendMany`.
Replaced with `buildFrontendMsgsConcat`, which fuses all messages
into a single `Builder`, materializes once, and sends with a single
`send()`:

```haskell
-- Before: N toStrict calls, sendMany with N iovecs
sendFrontendMsgs wc msgs = do
  let chunks = map buildFrontendMsg msgs
  wcSendMany wc chunks

-- After: 1 toStrict call, 1 send
sendFrontendMsgs wc msgs = do
  let bytes = buildFrontendMsgsConcat msgs
  wcSend wc bytes
```

For a typical Bind+Execute+Sync batch (~100 bytes total), this
eliminates 2 intermediate `ByteString` allocations and replaces
`sendMany` (writev with 3 iovecs) with a single `send`.

**Direct-to-Vector DataRow parsing.** `parseDataRow` previously built
a `[Maybe ByteString]` list via recursive `parseColValues`, then
converted to a Vector via `V.fromListN`. The list is O(n) cons cells
immediately consumed. Replaced with `V.create` using a mutable vector:

```haskell
-- Before: list intermediate
(vals, _) <- parseColValues n (BS.drop 2 bs)
Right (DataRow (V.fromListN n vals))

-- After: direct mutable write, validation + fill passes
vals <- parseColsDirect n bs 2
Right (DataRow vals)
```

The validation pass checks all column lengths fit within the buffer
(safety), then the fill pass uses `unsafeIndex`/`unsafeTake`/
`unsafeDrop` for zero-copy column slicing. For a 20-column row, this
saves 20 cons cells per row.

*Result: eliminated per-message and per-row intermediate allocations
in the two highest-frequency code paths.*

### Summary: encode path improvement

| Stage | Int32 encode | Cumulative |
|-------|-------------|------------|
| Initial (Builder pipeline) | 177 ns | 1x |
| + INLINE pragmas | ~160 ns | 1.1x |
| + -funbox-strict-fields | ~140 ns | 1.3x |
| + unsafeCreate direct writes | **30 ns** | **5.9x** |

### Summary: full-stack improvements

| Layer | Technique | Impact |
|-------|-----------|--------|
| Encode | Direct byte writes (`unsafeCreate`) | 5-6x per value |
| Decode | Unrolled int64, bang patterns | 1.2-1.4x per value |
| Protocol | Pre-computed message sizes | Eliminated double-copy |
| Protocol | Parse+Bind+Execute coalescing | First-exec latency halved |
| Protocol | Flush instead of Sync for preparation | 1 fewer message per cold stmt |
| Wire | Merged 5-byte header recv | 1 fewer syscall/message |
| Wire | TCP_NODELAY + message coalescing | Latency parity with C |
| Async | Sender/receiver split (writer+reader threads) | 7.2x at 32 concurrent threads |
| Async | Reader error recovery | Query errors no longer kill connection |
| Execute | Fused decode + DList accumulation | Eliminated intermediate list + reverse |
| Batch | Pipelined Bind+Execute | 40-100x for N inserts |
| Batch | Streaming chunks for large batches | Bounded memory regardless of size |
| Alloc | Constant format vectors (`binaryFmtVec`) | Eliminated per-query Vector alloc |
| Encode | Fused Builder (`buildFrontendMsgsConcat`) | 1 alloc + 1 send per batch, not N |
| Decode | Direct-to-Vector DataRow (`V.create`) | Eliminated per-row list intermediate |
| Strictness | Bang patterns, foldl', UNPACK | Zero space leaks |
| Safety | -fno-full-laziness, K8K stack tests | Regression-proof |

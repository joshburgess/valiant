# Performance

hsqlx implements its own PostgreSQL wire protocol in pure Haskell with binary
format encoding. This document covers the benchmark results, the techniques
that make it fast, and the optimization journey.

## Benchmark results

All benchmarks run on the same machine against Docker Postgres 16 on localhost.
Single connection, no connection pool overhead. Compared against
[hasql](https://hackage.haskell.org/package/hasql) (libpq FFI, binary format)
and [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)
(libpq FFI, text format).

### Read performance

| Rows | hsqlx | hasql | pg-simple | vs hasql | vs pg-simple |
|------|-------|-------|-----------|----------|--------------|
| 1 (by PK) | 0.97 ms | 0.99 ms | 1.06 ms | **faster** | **9% faster** |
| 1,000 | 4.5 ms | 7.4 ms | 8.3 ms | **39% faster** | **46% faster** |
| 5,000 | 19.6 ms | 37.5 ms | 41.5 ms | **48% faster** | **53% faster** |
| 10,000 | 37.2 ms | 74.5 ms | 81.7 ms | **50% faster** | **54% faster** |

hsqlx is the fastest Haskell PostgreSQL library for reads. The advantage
grows with row count because the per-row decode overhead is lower.

### Write performance

| Operation | hsqlx | hsqlx (pipelined) | hasql | pg-simple |
|-----------|-------|-------------------|-------|-----------|
| INSERT 100 rows | 104 ms | **2.5 ms** | 104 ms | 115 ms |
| INSERT 1,000 rows | 942 ms | **11.7 ms** | 926 ms | 1.20 s |
| INSERT 5,000 rows | — | **48.8 ms** | 4.92 s | 4.84 s |

Pipelined batch inserts (`executeBatch`) are **40-100x faster** than
sequential inserts with any library.

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

### 4. Message coalescing

When sending Bind+Execute+Sync (or any sequence of messages), hsqlx
concatenates all protocol messages into a single `send()` syscall via
`sendFrontendMsgs`. Combined with `TCP_NODELAY`, this eliminates the
latency penalty of small messages.

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
- `V.fromListN` with known size for `DataRow` parsing and `ToParams`
  tuple encoding (Vector pre-allocates exact buffer)
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
# Start a test Postgres instance
eval $(scripts/pg-setup.sh)

# Codec benchmarks (pure, no database needed)
cabal bench hsqlx-bench

# Comparative benchmarks vs hasql and postgresql-simple
cabal run bench-compare

# Teardown
scripts/pg-teardown.sh
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

### Pass 2: Strictness audit (plugin-assisted)

A comprehensive strictness audit using the `haskell-strictness` plugin
identified and fixed every lazy accumulation pattern in the codebase.

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

The final pass targeted the protocol encoding and wire framing layers.

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
| Wire | Merged 5-byte header recv | 1 fewer syscall/message |
| Wire | TCP_NODELAY + message coalescing | Latency parity with C |
| Execute | Fused decode + DList accumulation | Eliminated intermediate list + reverse |
| Batch | Pipelined Bind+Execute | 40-100x for N inserts |
| Strictness | Bang patterns, foldl', UNPACK | Zero space leaks |
| Safety | -fno-full-laziness, K8K stack tests | Regression-proof |

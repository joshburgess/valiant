# Known Gaps and Future Work

This document tracks features missing from the hsqlx/pg-wire
implementation, performance improvements, and architectural changes
planned for future releases.

Last updated: 2026-03-17

---

## Completed

- **TLS support** — SSLRequest subprotocol, TLS handshake via `tls`,
  TlsDisable/TlsPrefer/TlsRequire modes
- **SCRAM channel binding** — SCRAM-SHA-256-PLUS with tls-server-end-point
- **Query cancellation** — `cancelQuery`, `withQueryTimeout`
- **Connection timeouts** — `ccConnectTimeout`, `ccQueryTimeout`
- **Savepoints** — `withSavepoint` with auto rollback/release
- **Prepared statement eviction** — LRU cache, max 256 per connection
- **Parameterized cursors** — Extended query protocol for DECLARE
- **UUID binary codec** — 16-byte round-trip via `uuid-types`
- **JSON/JSONB binary codec** — json (raw UTF-8) and jsonb (version byte)
- **INLINE pragmas** — All codec, protocol, and wire hot paths
- **TCP_NODELAY** — Disabled Nagle's on all connections
- **Message coalescing** — `sendFrontendMsgs` batches into single send
- **Pipelined batch writes** — `executeBatch` (40-100x faster)
- **Pipelined reads** — `Pipeline` Applicative (1.6-3.7x faster)
- **Pipelined batch reads** — `fetchBatchOne`/`fetchBatchAll`
- **UNNEST batch fetch** — `fetchByIds` with array parameter
- **Connection health checking** — Empty query validation before reuse
- **Comprehensive PgError** — All 17 protocol error fields
- **Direct byte writes** — `unsafeCreate` for fixed-size encodes (5.9x)
- **Fused row decoding** — `collectAndDecodeRows` eliminates intermediate list
- **Pre-computed message sizes** — Single-pass encoding, no double-copy
- **Merged 5-byte header recv** — Tag+length in one syscall
- **Unrolled int64 decode** — 8 direct reads, no loop
- **Difference lists** — O(1) append in row collection
- **V.fromListN** — Pre-allocated vectors in DataRow and ToParams
- **Strictness audit** — Bang patterns, foldl', UNPACK, -funbox-strict-fields,
  -fspecialise-aggressively, -K8K test stack limits
- **-fno-full-laziness** — Safety guard on streaming modules
- **Scientific encoder rewrite** — ByteString instead of String
- **GHC 9.10.3 upgrade** — Core libraries build on latest LTS GHC

---

## Critical: Architectural

### Sender/receiver split (automatic pipelining)

**Status:** Not implemented. Current design is serial: one thread sends
a request and waits for the response before the next thread can use the
connection.

**Impact:** When multiple green threads share a connection (via pool),
they serialize on the connection. With a sender/receiver split, multiple
threads' queries would be automatically batched into a single send,
providing implicit pipelining without any explicit `Pipeline` API.

**Implementation:** Three green threads per connection:
1. Application threads put `Request` into a `TBQueue`, block on `MVar`
2. Writer thread drains `TBQueue`, batches into one `sendMany`, enqueues
   response `MVar`s into a `TQueue`
3. Reader thread reads from socket, fills `MVar`s in order

```haskell
data Connection = Connection
  { connSendChan :: !(TBQueue Request)
  , connReader   :: !(Async Void)
  , connWriter   :: !(Async Void)
  , connPending  :: !(TQueue (MVar Response))
  }
```

This is the architecture behind asyncpg's 3x advantage over psycopg2 on
concurrent workloads. It would make our `Pipeline` API unnecessary for
the concurrent case — any green threads hitting the same connection
would get automatic pipelining for free.

**Effort:** Large. Requires rearchitecting `Connection`, `Execute`,
and how the pool dispatches work. Touches nearly every module.

---

### Vectored I/O (`sendMany`)

**Status:** We use `BS.concat (map buildFrontendMsg msgs)` which copies
all message bytes into one contiguous buffer before sending.

**Impact:** `Network.Socket.ByteString.sendMany` does scatter-gather I/O
— sends multiple `ByteString` chunks in a single syscall without copying
them together. Eliminates the `BS.concat` allocation on every pipelined
send.

**Implementation:** Replace `sendFrontendMsgs` to use `sendMany` with
the `Builder`'s lazy `ByteString` chunks directly:

```haskell
sendFrontendMsgs wc msgs =
  NSB.sendMany (wcSocket wc)
    (LBS.toChunks (B.toLazyByteString (foldMap encodeFrontendMsg msgs)))
```

**Effort:** Small. Requires exposing the socket from `WireConn`.

---

## High Priority: Performance

### `unsafeIndex` in decode hot paths

**Status:** All `BS.index` calls in decoders are bounds-checked.

**Impact:** In the decode hot path (every column of every row), we pay
for bounds checking that is redundant — the message length has already
been validated during framing. On millions of rows, this adds up.

**Implementation:** Replace `BS.index` with `Data.ByteString.Unsafe.unsafeIndex`
in `decodeInt16BE`, `decodeInt32BE`, `decodeInt64BE`, and the DataRow parser.
Leave bounds checks in the message framing layer where they protect
against malformed input.

**Effort:** Small.

---

### Pinned ByteStrings for socket writes

**Status:** Our `ByteString`s may be unpinned. GHC's socket send
functions need pinned memory, causing an implicit copy to pinned
memory on every send.

**Impact:** Extra allocation + copy on every socket write.

**Implementation:** Use `Data.ByteString.Internal.create` (which
allocates pinned) for the write path, or use `mallocBytes` +
`unsafePackMallocCStringLen` for zero-copy pinned allocation.

**Effort:** Small-medium.

---

### RowFold (constant-memory streaming without cursors)

**Status:** Large result sets require cursors (`withCursor`/`fetchBatch`)
which need a transaction. No fold-based streaming.

**Impact:** Cannot process million-row results in constant memory
without an explicit transaction and cursor.

**Implementation:**
```haskell
data RowFold a b = RowFold
  { foldInit    :: !b
  , foldStep    :: !(b -> a -> b)
  , foldExtract :: !(b -> IO b)
  }

executeWithFold :: Connection -> Statement p a -> p -> RowFold a b -> IO b
```

Decode and fold each `DataRow` as it arrives from the wire. No list,
no buffering. Works outside transactions.

**Effort:** Medium.

---

### Cross-connection statement cache sharing

**Status:** Each connection maintains its own independent statement
cache. When a connection is returned to the pool and a new one is
acquired, the new connection re-prepares all statements.

**Impact:** Repeated `Parse` round-trips for the same statements across
different connections from the same pool.

**Implementation:** Share a `TVar (HashMap Word64 ByteString)` across
all pool connections mapping SQL hash → server-side statement name.
When connection A prepares a statement, register it. When connection B
needs the same statement, skip Parse and go straight to Bind+Execute.
Requires consistent naming scheme across connections.

**Effort:** Medium.

---

### Statement cache key hashing

**Status:** Statement cache uses `Map ByteString ByteString` keyed by
the full SQL text. Lookup is O(n) in key length per comparison.

**Impact:** For long SQL strings, map lookups are slower than necessary.

**Implementation:** Hash SQL text to `Word64` using FNV-1a or xxHash.
Use `HashMap Word64 ByteString` for O(1) expected lookup.

**Effort:** Small.

---

## Medium Priority: Features

### Multi-host failover

**Status:** Connection strings only support a single host.

**Impact:** Cannot connect to HA Postgres setups with multiple hosts.

**Implementation:** Parse multiple hosts from the connection string,
attempt connection to each in order, fall back on failure. Support
`target_session_attrs` to distinguish primary vs standby.

---

### Binary COPY format

**Status:** COPY IN/OUT works with text and CSV formats.

**Impact:** Binary COPY is the fastest possible Postgres ingest path.
The binary format matches the column data format in `DataRow`, so the
same codec infrastructure is reused.

**Implementation:** Write binary COPY header (signature + flags + header
extension), then encode each row as binary tuple data using existing
`pgEncode` instances.

---

### Single-row mode

**Status:** Results are fully collected before returning.

**Impact:** Cannot process rows one-at-a-time for very large result sets
without cursors.

**Implementation:** Send `Execute` with `max_rows=1` and process each
`DataRow` as it arrives via a callback or fold.

---

### Protocol tracing

**Status:** No built-in way to log raw protocol messages.

**Implementation:** Add a trace callback to `WireConn`:
```haskell
wcTrace :: Maybe (Direction -> ByteString -> IO ())
```

---

### Mock server for testing

**Status:** All tests run against real Postgres.

**Impact:** Slower tests, harder to test edge cases.

**Implementation:** Build a minimal Postgres-protocol fake server using
the same `pg-wire` codec library. Enables QuickCheck fuzzing of the
client against malformed/unexpected server responses.

---

## Nice to Have

### GSSAPI/SSPI authentication

Rarely needed outside enterprise. Requires the `gssapi` package.

### Large object API

`lo_create`/`lo_read`/`lo_write`. Rarely used in modern applications.

### Conduit/streaming integration

Optional extension packages (`hsqlx-conduit`, `hsqlx-streaming`) for
integration with streaming ecosystems. Core library stays
dependency-minimal with `RowFold`.

### Pre-allocated receive buffer

Replace `recvExact`'s list accumulation with a pre-allocated
`MutableByteArray` or ring buffer.

### Unboxed vectors for fixed-size result columns

For columns of known fixed-size types, use unboxed vectors to avoid
per-value heap allocation.

### GHC plugin port to 9.10

The GHC source plugin currently requires GHC 9.4. Needs CPP
conditionals or `ghc-tcplugin-api` for the GHC API changes in 9.6-9.10.

---

## Recommended iteration order

1. **`unsafeIndex` in decode paths** — small, immediate perf win
2. **Vectored I/O (`sendMany`)** — small, eliminates concat on send
3. **RowFold streaming** — medium, enables constant-memory large results
4. **Binary COPY format** — medium, fastest bulk ingest
5. **Cross-connection statement cache** — medium, reduces Parse round-trips
6. **Statement cache hashing** — small, faster lookups
7. **Sender/receiver split** — large, transformative for concurrent perf
8. **Multi-host failover** — medium, production necessity for HA
9. **Protocol tracing** — small, debugging aid
10. **GHC plugin 9.10 port** — medium, ecosystem compatibility

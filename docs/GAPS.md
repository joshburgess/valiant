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

## libpq Feature Parity

Comprehensive comparison against every libpq feature area from the
PostgreSQL 16 documentation (Chapter 34). Items marked HAVE are
implemented. Items marked NEED are required for feature parity.
Items marked SKIP are intentionally omitted (C-specific, deprecated,
or not applicable to a pure Haskell driver).

### Connection (34.1)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQconnectdb` (connection string) | HAVE | `connectString` |
| `PQfinish` (close) | HAVE | `close` |
| URI format parsing | HAVE | `postgres://user:pass@host:port/db` |
| Key-value format parsing | HAVE | `host=x port=y dbname=z` |
| `host` parameter | HAVE | |
| `port` parameter | HAVE | |
| `dbname` parameter | HAVE | |
| `user` parameter | HAVE | |
| `password` parameter | HAVE | |
| `connect_timeout` parameter | HAVE | `ccConnectTimeout` |
| `application_name` parameter | HAVE | `ccAppName` |
| `sslmode` (disable/prefer/require) | HAVE | `TlsMode` |
| `sslmode` verify-ca | NEED | Requires CA cert path config |
| `sslmode` verify-full | NEED | Requires hostname verification |
| `sslcert` (client certificate) | NEED | Client cert authentication |
| `sslkey` (client key) | NEED | Client key for mutual TLS |
| `sslrootcert` (CA file) | NEED | Custom CA certificate |
| `sslcrl` / `sslcrldir` | SKIP | CRL checking, rarely used |
| `sslsni` (Server Name Indication) | NEED | SNI for TLS routing |
| `ssl_min_protocol_version` | NEED | TLS version pinning |
| `channel_binding` parameter | HAVE | `scramAuthWithChannelBinding` |
| `target_session_attrs` | NEED | primary/standby/read-write/read-only |
| `load_balance_hosts` | NEED | Random host ordering |
| Multi-host (`host=a,b,c`) | NEED | Failover across hosts |
| `keepalives` parameters | NEED | TCP keepalive config |
| `tcp_user_timeout` | NEED | Unacked data timeout |
| `client_encoding` | NEED | Auto-detect or set encoding |
| `options` (startup options) | NEED | Server command-line options |
| `passfile` (~/.pgpass) | NEED | Password file lookup |
| `service` (pg_service.conf) | SKIP | Service file lookup |
| `PQconnectStart` / `PQconnectPoll` | SKIP | Non-blocking connect (GHC green threads handle this) |
| `PQreset` (reconnect) | NEED | Reset connection without closing |
| `PQping` (server status check) | NEED | Check if server is alive |
| `PQconndefaults` | SKIP | C-specific default enumeration |
| `gssencmode` / `krbsrvname` / `gsslib` | SKIP | GSSAPI (enterprise only) |
| `replication` parameter | SKIP | Streaming replication |

### Connection Status (34.2)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQstatus` (connection status) | NEED | Public API for connection state |
| `PQtransactionStatus` | HAVE | `connTxStatus` (internal IORef) |
| `PQparameterStatus` | HAVE | `connParams` (internal IORef) |
| `PQserverVersion` | NEED | Parse from server_version param |
| `PQbackendPID` | HAVE | `connBackendPid` |
| `PQsocket` | SKIP | C-specific (we own the socket) |
| `PQerrorMessage` | HAVE | Via `PgError` |
| `PQprotocolVersion` | SKIP | We always use v3 |
| `PQsslInUse` | NEED | Query whether TLS is active |
| `PQsslAttribute` | NEED | TLS cipher, protocol version info |

### Command Execution (34.3)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQexec` (simple query) | HAVE | `simpleQuery` |
| `PQexecParams` (parameterized) | HAVE | `fetchOne`/`fetchAll`/`execute` |
| `PQprepare` | HAVE | `ensurePrepared` (internal) |
| `PQexecPrepared` | HAVE | All execute functions |
| `PQdescribePrepared` | NEED | Get param/result types for a prepared stmt |
| `PQdescribePortal` | SKIP | Rarely used directly |
| Result status codes | HAVE | `CommandTag`, `ErrorResponse` |
| `PQresultErrorField` | HAVE | All 17 `PgError` fields |
| `PQntuples` / `PQnfields` | HAVE | Via `Vector` length |
| `PQfname` / `PQftype` / etc. | HAVE | `FieldInfo` in `RowDescription` |
| Binary format results | HAVE | Default for all queries |
| `PQcmdTuples` (rows affected) | HAVE | `execute` return value |
| `PQescapeLiteral` | NEED | SQL literal escaping |
| `PQescapeIdentifier` | NEED | SQL identifier escaping |
| `PQescapeByteaConn` | SKIP | Only needed for text protocol |
| `PQunescapeBytea` | SKIP | Only needed for text protocol |

### Asynchronous Command Processing (34.4)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQsendQuery` / `PQgetResult` | NEED | Non-blocking query + collect |
| `PQconsumeInput` | SKIP | C-specific (we use non-blocking IO) |
| `PQisBusy` | SKIP | C-specific polling |
| `PQsetnonblocking` | SKIP | GHC handles this natively |

### Pipeline Mode (34.5)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQpipelineStatus` | SKIP | Our pipelining is implicit |
| `PQenterPipelineMode` | HAVE | `executeBatch`, `Pipeline`, `fetchBatchOne` |
| `PQexitPipelineMode` | HAVE | Automatic after Sync |
| `PQpipelineSync` | HAVE | Single Sync at end of batch |
| `PQsendFlushRequest` | SKIP | We always Sync |

### Row-by-Row Processing (34.6)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQsetSingleRowMode` | NEED | Process rows as they arrive |

### Query Cancellation (34.7)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQcancel` | HAVE | `cancelQuery` |
| `PQrequestCancel` (deprecated) | SKIP | |

### Fast-Path Interface (34.8)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQfn` | SKIP | Deprecated, rarely used |

### Asynchronous Notification (34.9)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQnotifies` (non-blocking check) | NEED | Check without blocking |
| LISTEN/NOTIFY | HAVE | `listen`/`unlisten`/`waitForNotification` |

### COPY (34.10)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQputCopyData` | HAVE | `copyIn` |
| `PQputCopyEnd` | HAVE | `CopyDone` |
| `PQgetCopyData` | HAVE | `copyOut` |
| Binary COPY format | NEED | Header + binary tuple data |

### Control Functions (34.11)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQsetErrorVerbosity` | SKIP | We always parse all fields |
| `PQsetErrorContextVisibility` | SKIP | |
| `PQtrace` / `PQuntrace` | NEED | Protocol tracing |

### Miscellaneous (34.12)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQencryptPasswordConn` | NEED | Create encrypted passwords |
| `PQlibVersion` | SKIP | C-specific |

### Notice Processing (34.13)

| Feature | Status | Notes |
|---------|--------|-------|
| Notice handler callback | NEED | User-configurable notice handler |

### SSL Support (34.19)

| Feature | Status | Notes |
|---------|--------|-------|
| Server certificate verification | NEED | verify-ca, verify-full |
| Client certificates | NEED | sslcert + sslkey |
| TLS 1.2/1.3 | HAVE | Via `tls` library |
| System cert store | HAVE | `getSystemCertificateStore` |

### Summary: Items needed for feature parity

**Must have (production blockers):**
1. SSL: verify-ca, verify-full, sslcert, sslkey, sslrootcert
2. Multi-host failover with target_session_attrs
3. SQL escaping (escapeLiteral, escapeIdentifier)
4. Connection reset (PQreset equivalent)
5. Server version query
6. Connection status API (public accessors)
7. Non-blocking notification check
8. Notice handler callback
9. TCP keepalive configuration
10. PQdescribePrepared equivalent

**Should have:**
11. Single-row mode / RowFold
12. Binary COPY format
13. Protocol tracing
14. Password encryption (PQencryptPasswordConn)
15. client_encoding parameter
16. Startup options parameter
17. Password file (~/.pgpass) support
18. TLS introspection (sslInUse, sslAttribute)
19. SNI (Server Name Indication)
20. TLS version pinning (ssl_min/max_protocol_version)
21. load_balance_hosts (random ordering)
22. PQping (server alive check)

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

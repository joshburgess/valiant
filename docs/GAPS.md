# Known Gaps and Future Work

This document tracks features missing from the valiant/pg-wire
implementation, performance improvements, and architectural changes
planned for future releases.

Last updated: 2026-03-24

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
- **Sender/receiver split** — Dedicated writer+reader threads per connection,
  automatic pipelining for concurrent workloads, 7.2x scaling at 32 threads
- **Vectored I/O** — `sendMany` for scatter-gather, then fused Builder
  encoding (`buildFrontendMsgsConcat`) for single-allocation single-syscall sends
- **`unsafeIndex` in decode paths** — Bounds-checked at framing layer,
  unchecked in per-column hot path
- **Cross-connection statement cache** — Pool-level `TVar (Map Int ByteString)`
  shared across connections; connection B skips Parse if A already prepared
- **Statement cache key hashing** — `Hashable`-based Int keys instead of
  full SQL text comparison
- **RowFold** — `executeWithFold` for constant-memory streaming without cursors
- **Binary COPY format** — `copyInBinary` with binary tuple encoding
- **Protocol tracing** — `setTraceHandler` callback on raw wire bytes
- **Multi-host failover** — Comma-separated hosts, `target_session_attrs`,
  `load_balance_hosts` (Fisher-Yates shuffle)
- **Parse+Bind coalescing** — First-execution single round-trip
- **Flush-based preparation** — `Parse + Flush` instead of `Parse + Sync`
- **Constant format vectors** — Module-level CAF for `binaryFmtVec`
- **Streaming batch execution** — Chunks of 256 for large batches, bounded memory
- **Reader error recovery** — Query errors are per-request, not connection-fatal
- **Fused Builder encoding** — `buildFrontendMsgsConcat` fuses N messages into
  one Builder pass, one `toStrict`, one `send()`
- **Direct-to-Vector DataRow parsing** — `V.create` with mutable vector,
  validation pass then unsafe fill pass, eliminates intermediate list
- **Pool-level TypeCache** — `TVar (Map Word32 TypeInfo)` shared across
  connections for resolved PG type metadata
- **Auto PG type discovery** — `valiant prepare` queries `pg_type`/`pg_enum`/
  `pg_range` for unknown OIDs instead of failing; enums→Text, domains→unwrap,
  ranges→PgRange
- **PgEnum type class** — `Valiant.Binary.Enum` codec for enum binary format
- **Extended cache format** — `pg_type_category` and `pg_enum_labels` in
  cache JSON (backward-compatible optional fields)
- **`-Werror`** — All four cabal packages compile warning-free
- **Named parameters** — `:name` syntax in SQL files, `ToNamedParams` type class,
  `mkStatementNamed` constructor, Generic-based record encoding with field
  reordering and clear mismatch error messages
- **Valiant.Monad** — `type Valiant = ReaderT Pool IO` convenience monad with
  lifted `fetchOneM`, `fetchAllM`, `executeM`, `withTransactionM`, etc.

---

## libpq Feature Parity

Comprehensive comparison against every libpq feature area from the
PostgreSQL 16 documentation (Chapter 34). All production-critical
features are implemented. Items marked SKIP include detailed
rationale for why they are intentionally omitted.

### Connection (34.1)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQconnectdb` (connection string) | HAVE | `connectString` |
| `PQfinish` (close) | HAVE | `close` |
| URI format parsing | HAVE | `postgres://user:pass@host:port/db` |
| Key-value format parsing | HAVE | `host=x port=y dbname=z` |
| `host` parameter | HAVE | Including comma-separated multi-host |
| `port` parameter | HAVE | |
| `dbname` parameter | HAVE | |
| `user` parameter | HAVE | |
| `password` parameter | HAVE | |
| `connect_timeout` parameter | HAVE | `ccConnectTimeout` |
| `application_name` parameter | HAVE | `ccAppName` |
| `sslmode` (all 5 modes) | HAVE | disable/prefer/require/verify-ca/verify-full |
| `sslcert` (client certificate) | HAVE | `ccSslCert` |
| `sslkey` (client key) | HAVE | `ccSslKey` |
| `sslrootcert` (CA file) | HAVE | `ccSslRootCert` |
| `sslsni` (Server Name Indication) | HAVE | Sent by `tls` library automatically |
| `ssl_min_protocol_version` | HAVE | Via `supportedVersions` in TLS config |
| `channel_binding` parameter | HAVE | `scramAuthWithChannelBinding` |
| `target_session_attrs` | HAVE | All 6 values (any/read-write/read-only/primary/standby/prefer-standby) |
| `load_balance_hosts` | HAVE | `ccLoadBalanceHosts` (Fisher-Yates shuffle) |
| Multi-host (`host=a,b,c`) | HAVE | Failover with `target_session_attrs` |
| `keepalives` parameters | HAVE | idle/interval/count in `ConnConfig` |
| `client_encoding` | HAVE | `ccClientEncoding` |
| `options` (startup options) | HAVE | `ccOptions` |
| `passfile` (~/.pgpass) | HAVE | `lookupPgpass` |
| `PQreset` (reconnect) | HAVE | `reset` |
| `PQping` (server status check) | HAVE | `ping` |
| `sslcrl` / `sslcrldir` | SKIP | See below |
| `service` (pg_service.conf) | SKIP | See below |
| `PQconnectStart` / `PQconnectPoll` | SKIP | See below |
| `PQconndefaults` | SKIP | See below |
| `gssencmode` / `krbsrvname` / `gsslib` | SKIP | See below |
| `replication` parameter | SKIP | See below |
| `tcp_user_timeout` | SKIP | See below |
| `requirepeer` | SKIP | See below |

### Connection Status (34.2)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQstatus` | HAVE | `connectionStatus` |
| `PQtransactionStatus` | HAVE | `transactionStatus` |
| `PQparameterStatus` | HAVE | `parameterStatus` |
| `PQserverVersion` | HAVE | `serverVersion` |
| `PQbackendPID` | HAVE | `backendPid` |
| `PQerrorMessage` | HAVE | Via `PgError` |
| `PQsslInUse` | HAVE | `isSslInUse` |
| `PQsocket` | SKIP | See below |
| `PQprotocolVersion` | SKIP | See below |
| `PQsslAttribute` | SKIP | See below |

### Command Execution (34.3)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQexec` (simple query) | HAVE | `simpleQuery` |
| `PQexecParams` (parameterized) | HAVE | `fetchOne`/`fetchAll`/`execute` |
| `PQprepare` | HAVE | `ensurePrepared` (internal, auto-cached) |
| `PQexecPrepared` | HAVE | All execute functions |
| `PQdescribePrepared` | HAVE | `describePrepared` |
| Result status codes | HAVE | `CommandTag`, `ErrorResponse` |
| `PQresultErrorField` | HAVE | All 17 `PgError` fields |
| `PQntuples` / `PQnfields` | HAVE | Via `Vector` length |
| `PQfname` / `PQftype` / etc. | HAVE | `FieldInfo` in `RowDescription` |
| Binary format results | HAVE | Default for all queries |
| `PQcmdTuples` (rows affected) | HAVE | `execute` return value |
| `PQescapeLiteral` | HAVE | `escapeLiteral` |
| `PQescapeIdentifier` | HAVE | `escapeIdentifier` |
| `PQdescribePortal` | SKIP | See below |
| `PQescapeByteaConn` | SKIP | See below |
| `PQunescapeBytea` | SKIP | See below |

### Asynchronous Command Processing (34.4)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQsendQuery` / `PQgetResult` | HAVE | Via `Pipeline` Applicative + `fetchBatchOne`/`fetchBatchAll` |
| `PQconsumeInput` | SKIP | See below |
| `PQisBusy` | SKIP | See below |
| `PQsetnonblocking` | SKIP | See below |

### Pipeline Mode (34.5)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQenterPipelineMode` | HAVE | `executeBatch`, `Pipeline`, `fetchBatchOne` |
| `PQexitPipelineMode` | HAVE | Automatic after Sync |
| `PQpipelineSync` | HAVE | Single Sync at end of batch |
| `PQpipelineStatus` | SKIP | See below |
| `PQsendFlushRequest` | SKIP | See below |

### Row-by-Row Processing (34.6)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQsetSingleRowMode` | HAVE | `RowFold` + `executeWithFold` (constant-memory streaming) |

### Query Cancellation (34.7)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQcancel` | HAVE | `cancelQuery` + `withQueryTimeout` |
| `PQrequestCancel` (deprecated) | SKIP | See below |

### Fast-Path Interface (34.8)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQfn` | SKIP | See below |

### Asynchronous Notification (34.9)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQnotifies` (non-blocking check) | HAVE | `checkNotification` |
| LISTEN/NOTIFY | HAVE | `listen`/`unlisten`/`waitForNotification`/`waitForNotificationTimeout` |

### COPY (34.10)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQputCopyData` | HAVE | `copyIn` (text/CSV) + `copyInBinary` (binary) |
| `PQputCopyEnd` | HAVE | `CopyDone` |
| `PQgetCopyData` | HAVE | `copyOut` |
| Binary COPY format | HAVE | `copyInBinary` with binary tuple encoding |

### Control Functions (34.11)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQtrace` / `PQuntrace` | HAVE | `setTraceHandler` (callback on raw bytes) |
| `PQsetErrorVerbosity` | SKIP | See below |
| `PQsetErrorContextVisibility` | SKIP | See below |

### Miscellaneous (34.12)

| Feature | Status | Notes |
|---------|--------|-------|
| `PQencryptPasswordConn` | HAVE | `encryptPassword` (MD5 format) |
| `PQlibVersion` | SKIP | See below |

### Notice Processing (34.13)

| Feature | Status | Notes |
|---------|--------|-------|
| Notice handler callback | HAVE | `setNoticeHandler` |

### Event System (34.14)

| Feature | Status | Notes |
|---------|--------|-------|
| `PGEVT_*` callbacks | SKIP | See below |

### SSL Support (34.19)

| Feature | Status | Notes |
|---------|--------|-------|
| Server certificate verification | HAVE | verify-ca, verify-full modes |
| Client certificates | HAVE | `ccSslCert` + `ccSslKey` |
| TLS 1.2/1.3 | HAVE | Via `tls` library |
| System cert store | HAVE | `getSystemCertificateStore` |

---

### Why items are marked SKIP

Each skipped item has a specific technical reason for omission.

**`sslcrl` / `sslcrldir`** (Certificate Revocation Lists) —
CRL checking is a legacy mechanism for verifying that a TLS certificate
hasn't been revoked before its expiration date. In practice, almost
nobody uses CRL files anymore — OCSP stapling (where the server
provides revocation status during the TLS handshake) has replaced CRLs.
Even libpq's own documentation notes this is rarely configured. The
`tls` library could support CRL loading but the implementation effort
is disproportionate to the number of deployments that use it.

**`service` (pg_service.conf)** —
A configuration file that maps service names to connection parameters,
allowing `service=myapp` instead of a full connection string. This is
a convenience feature for operations teams managing many database
connections. Most modern deployments use environment variables
(`DATABASE_URL`) or direct connection strings. Not hard to implement
but very niche usage.

**`PQconnectStart` / `PQconnectPoll`** (Non-blocking connect) —
In C, you need these because `connect()` blocks the calling OS thread.
In GHC, the runtime's I/O manager handles this automatically — when
`Network.Socket.connect` blocks, only the lightweight green thread
sleeps, while the OS thread (capability) is freed for other green
threads. We get non-blocking connect behavior for free from GHC's
runtime. Implementing this API would be pointless busywork that
duplicates what the runtime already provides.

**`PQconndefaults`** —
Returns a list of all connection parameter keywords with their compiled
defaults, environment variable names, and descriptions. This is a
C-specific introspection API for tools that need to enumerate libpq's
configuration options at runtime. In Haskell, `defaultConnConfig` serves
the same purpose as a regular data value, and the `ConnConfig` type
documents all fields via Haddock.

**`gssencmode` / `krbsrvname` / `gsslib`** (GSSAPI/Kerberos) —
These enable Kerberos single sign-on authentication, used in corporate
Active Directory environments where users authenticate via their
Windows/domain credentials instead of a database password. This is
rarely seen outside Fortune 500, government, and financial institution
environments. Implementing it would require the `gssapi` Haskell
package, which wraps MIT Kerberos C libraries via FFI — breaking our
"pure Haskell, no C dependencies" design goal.

**`replication` parameter** (Streaming replication) —
This enables the PostgreSQL streaming replication protocol, used by
`pg_basebackup`, Patroni, and replication management tools to stream
Write-Ahead Log (WAL) records from a primary to replicas. It's a
completely different protocol from the normal query protocol and is
not used by application database drivers. No Haskell database library
implements this — it's the domain of infrastructure tools, not
application code.

**`tcp_user_timeout`** —
Sets a timeout in milliseconds for unacknowledged TCP data. This is
a Linux-specific socket option (`TCP_USER_TIMEOUT`) that doesn't exist
on macOS or Windows. It could be implemented as a best-effort option
on Linux, but the portability concerns make it low priority. Our
`ccConnectTimeout` and `withQueryTimeout` cover the practical timeout
use cases.

**`requirepeer`** —
Requires the server process to be running as a specific OS user name.
This only works over Unix domain socket connections and is a niche
security feature for local-only deployments where the DBA wants to
verify the server's identity at the OS level.

**`PQsocket`** —
Returns the file descriptor of the underlying socket. In libpq this
is needed for integrating with external event loops (epoll, kqueue).
In our architecture, we own the socket internally and GHC's I/O manager
handles event loop integration. Exposing the raw FD would break
encapsulation and serve no practical purpose.

**`PQprotocolVersion`** —
Returns the frontend/backend protocol version. We always use protocol
version 3 (the only modern version). There's nothing to query.

**`PQsslAttribute`** —
Returns TLS connection attributes like cipher name, protocol version,
and key bits. We have `isSslInUse` for the basic check. Full TLS
attribute introspection would require extracting information from the
`tls` library's `Context` type, which is possible but rarely needed
by applications.

**`PQdescribePortal`** —
Returns the result column types for a portal (a bound, ready-to-execute
statement). Portals are an intermediate protocol concept rarely used
directly — most applications work with prepared statements
(`describePrepared`) not portals.

**`PQescapeByteaConn` / `PQunescapeBytea`** (Binary data escaping) —
These exist because text-format drivers (like `postgresql-simple`) need
to escape binary data for embedding in SQL strings as hex or octal
escape sequences. We use binary format exclusively — binary data is
sent as raw bytes in Bind parameters with a length prefix. There is
nothing to escape.

**`PQconsumeInput` / `PQisBusy`** (C polling) —
These are the C-level mechanism for non-blocking query processing:
call `PQsendQuery`, then poll with `PQconsumeInput` + `PQisBusy`
in a loop until the result is ready. In Haskell, blocking on a socket
read puts only the green thread to sleep — GHC's I/O manager
(`threadWaitRead`) handles the polling automatically. Our blocking
`recvBackendMsg` is effectively non-blocking from the perspective of
other green threads.

**`PQsetnonblocking`** —
Tells libpq to use non-blocking socket operations. Same as the above:
GHC's I/O manager makes all socket operations non-blocking at the
green thread level. This flag has no equivalent because non-blocking
is the default behavior.

**`PQpipelineStatus` / `PQsendFlushRequest`** —
`PQpipelineStatus` returns whether libpq is in pipeline mode.
`PQsendFlushRequest` sends a Flush message (request server to send
pending results without a full Sync). Our pipelining is structural:
`executeBatch`, `Pipeline`, `fetchBatchOne`, and `fetchBatchAll`
handle pipeline entry/exit automatically, and we always Sync at the
end of a batch for clean protocol state.

**`PQrequestCancel`** (deprecated) —
The old cancellation API, replaced by `PQcancel`. We implement
`cancelQuery` which uses the modern CancelRequest protocol.

**`PQfn`** (Fast-Path Interface) —
Calls a server-side function by OID, bypassing the SQL parser.
Deprecated since PostgreSQL 7.4 (2003). The documentation explicitly
recommends using `PQexecParams` instead. No modern application uses
this.

**`PQsetErrorVerbosity` / `PQsetErrorContextVisibility`** —
Controls how much detail libpq includes when formatting error messages
as strings. We parse all 17 error fields from `ErrorResponse` into
structured `PgError` records — the caller decides what to display.
Verbosity control is unnecessary when you have the structured data.

**`PQlibVersion`** —
Returns the libpq library version number. We are not libpq. Users
can check the `pg-wire` package version via Cabal instead.

**`PGEVT_*` event system** (34.14) —
libpq's plugin system for hooking into connection lifecycle events
(connect, disconnect, result creation, result destruction, etc.).
This is a C-level extensibility mechanism using function pointers
registered in a callback table. It doesn't translate to Haskell —
we have first-class functions for everything the event system does:
`setNoticeHandler` for notices, `setTraceHandler` for protocol tracing,
bracket patterns for connection lifecycle, and the pool's health
checking and reaper for connection management. Haskell's own
abstraction mechanisms (higher-order functions, bracket, STM) are
strictly more powerful than a C callback table.

---

## Nice to Have

### GSSAPI/SSPI authentication

Rarely needed outside enterprise. Requires the `gssapi` package.

### Unboxed vectors for fixed-size result columns

For columns of known fixed-size types, use unboxed vectors to avoid
per-value heap allocation.

---

## Completed (from this list)

- Connection pool optimization (done: jitter, hooks, resize, drain, observations)
- GHC plugin 9.6/9.8/9.10 (done: CPP conditionals)
- Mock server (done: PgWire.MockServer)
- Pre-allocated receive buffer (done: 32KB chunks, Builder accumulation)
- Conduit/pipes/streaming/streamly/bluefin/effectful/fused-effects/mtl integration (done: 8 adapter packages)
- Large object API (done: `Valiant.LargeObject` with bracket, import/export, streaming)

---

## Future Work

### State machine testing for connection pool

Use `quickcheck-lockstep` or Hedgehog's built-in state machine testing
to formally verify pool behavior under arbitrary command sequences.

**Model:**
```haskell
data PoolModel = PoolModel
  { mIdle :: Int
  , mActive :: Int
  , mMaxSize :: Int
  , mClosed :: Bool
  }
```

**Commands:**
- `Acquire` — take a connection from the pool
- `Release` — return a connection
- `Close` — shut down the pool
- `Resize Int` — change max size
- `Tick` — simulate reaper sweep (expire idle connections)

**Postconditions (checked after every command):**
- `mIdle + mActive <= mMaxSize`
- `mActive >= 0 && mIdle >= 0`
- After `Close`: all subsequent `Acquire` returns `PoolClosed`
- After `Resize n`: `mMaxSize == n`
- After `Release`: `mActive` decreases by 1

**Why it matters:** The Hedgehog property tests we have verify invariants
after a fixed sequence of operations. State machine testing generates
*arbitrary interleaved sequences* of commands and checks invariants
after *every step*. This can find ordering-dependent bugs that fixed
sequences miss.

**Estimated effort:** 200-300 lines. Requires `quickcheck-lockstep` or
Hedgehog state machine API. Can run against the mock server (no DB).

**Reference:** Well-Typed's Haskell Unfolder Episode 44 (May 2025) on
`quickcheck-lockstep` for state-based testing.

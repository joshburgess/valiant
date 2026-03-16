# Known Gaps and Future Work

This document tracks features missing from the hsqlx wire protocol
implementation compared to libpq, as well as performance and ergonomic
improvements planned for future releases.

Last updated: 2026-03-16

---

## Critical

### TLS: SCRAM channel binding

**Status:** SCRAM-SHA-256 authentication is implemented, but channel binding
is hardcoded to non-channel-binding mode (`"n,,"`). This means
`tls-unique` and `tls-server-end-point` channel binding variants are
not supported.

**Impact:** Some Postgres configurations that require channel binding will
reject authentication.

**Implementation:** Parse the channel binding type from the SASL mechanism
name (`SCRAM-SHA-256-PLUS`), extract the channel binding data from the
TLS context via `Network.TLS.getFinished`, and include it in the SCRAM
exchange.

---

### Query cancellation

**Status:** `BackendKeyData` (PID + secret key) is received and stored on
the `Connection`, but there is no API to cancel an in-flight query.

**Impact:** Long-running queries cannot be cancelled. The only option is
to close the connection, which is destructive.

**Implementation:** Open a separate TCP connection to the same host/port,
send a `CancelRequest` message (not a normal frontend message — it has
its own format: `[length=16][code=80877102][pid][key]`), then close the
cancel connection. The original connection's query will receive an
`ErrorResponse` with SQLSTATE `57014` (query_canceled).

**API design:**
```haskell
cancelQuery :: Connection -> IO ()
-- or with timeout:
withQueryTimeout :: Connection -> NominalDiffTime -> IO a -> IO a
```

---

### Connection timeouts

**Status:** Socket read/write operations can block indefinitely. There is
no timeout on connect, query, or idle operations.

**Impact:** A hung Postgres server or network partition will cause the
application to hang forever.

**Implementation:**
- Connect timeout: use `System.Timeout.timeout` around `NS.connect`
- Query timeout: use `System.Timeout.timeout` around the execute cycle,
  combined with query cancellation to clean up the server side
- Socket-level: set `SO_RCVTIMEO`/`SO_SNDTIMEO` on the socket
- Add timeout fields to `ConnConfig` and `PoolConfig`

---

## Medium Priority

### Multi-host failover

**Status:** Connection strings only support a single host.

**Impact:** Cannot connect to HA Postgres setups with multiple hosts
(e.g., `host=primary,replica1,replica2`).

**Implementation:** Parse multiple hosts from the connection string,
attempt connection to each in order, fall back on failure. libpq supports
`target_session_attrs` to distinguish primary vs standby.

---

### Savepoints

**Status:** Transactions are top-level only (`BEGIN`/`COMMIT`/`ROLLBACK`).
No savepoint API.

**Impact:** Cannot do partial rollbacks within a transaction.

**Implementation:**
```haskell
withSavepoint :: Transaction -> (Transaction -> IO a) -> IO a
withSavepoint tx action = do
  let name = "sp_" <> uniqueId
  simpleQuery (txConn tx) ("SAVEPOINT " <> name)
  result <- action tx `onException` simpleQuery (txConn tx) ("ROLLBACK TO " <> name)
  simpleQuery (txConn tx) ("RELEASE " <> name)
  pure result
```

---

### Prepared statement eviction

**Status:** The per-connection statement cache (`connStmtCache`) grows
unboundedly. Every unique SQL string adds a new prepared statement that
is never deallocated.

**Impact:** Long-lived connections with many distinct queries will
accumulate server-side prepared statements, consuming memory on both
client and server.

**Implementation:** Add an LRU eviction policy. When the cache exceeds
a configurable limit (e.g., 256 statements), close the least-recently-used
statement via the `Close` protocol message.

---

### Parameterized cursors

**Status:** `Hsqlx.Streaming.withCursor` declares cursors using the simple
query protocol, which means parameters cannot be bound. Only
parameterless queries work with cursors.

**Impact:** Streaming queries with WHERE clauses require workarounds
(e.g., creating a temporary view).

**Implementation:** Use the extended query protocol for cursor declaration:
`DECLARE cursor_name CURSOR FOR $prepared_statement_name`, then bind
parameters via `Bind`.

---

### Asynchronous query submission

**Status:** All query functions are synchronous — they send the request
and block until the full response is received.

**Impact:** Cannot overlap query execution with other work on the same
connection.

**Implementation:** Split `executeExtended` into `sendQuery` (non-blocking)
and `receiveResult` (blocking). This enables patterns like:
```haskell
sendQuery conn stmt1 params1
sendQuery conn stmt2 params2
result1 <- receiveResult conn
result2 <- receiveResult conn
```

---

## Nice to Have

### GSSAPI/SSPI authentication

**Status:** Not implemented.

**Impact:** Cannot connect to Postgres instances that require Kerberos
or Windows Integrated Authentication.

**Notes:** Rarely needed outside enterprise environments. Would require
the `gssapi` Haskell package.

---

### Large object API

**Status:** Not implemented.

**Impact:** Cannot use PostgreSQL large objects (`lo_create`, `lo_read`,
`lo_write`, etc.).

**Notes:** Large objects are rarely used in modern applications. Most
use cases are better served by `bytea` columns or external storage.

---

### Protocol tracing

**Status:** Not implemented.

**Impact:** No built-in way to log raw protocol messages for debugging.

**Implementation:** Add a trace callback to `WireConn` that receives
the raw bytes and direction (send/recv) of each protocol message.
```haskell
data WireConn = WireConn
  { ...
  , wcTrace :: Maybe (Direction -> ByteString -> IO ())
  }
```

---

### Binary COPY format

**Status:** COPY IN/OUT works with text and CSV formats. Binary COPY
format (header + tuple data) is not implemented.

**Impact:** Binary COPY is faster for large bulk loads but requires
encoding rows in PG's binary tuple format.

---

### Single-row mode

**Status:** Not implemented. Results are always fully buffered before
returning.

**Impact:** Cannot process rows one-at-a-time for very large result sets
without using cursors.

**Implementation:** After `Bind`, send `Execute` with `max_rows=1` and
process each `DataRow` as it arrives.

---

## Performance Improvements

### Direct byte writing for fixed-size encodes

**Status:** Fixed-size type encoders (Int32, Int64, etc.) go through
`Builder → LazyByteString → toStrict`. For 4-8 bytes, this is
unnecessary overhead.

**Implementation:** Use `ByteString.Internal.unsafeCreate` with direct
`Ptr` writes:
```haskell
int32BE :: Int32 -> ByteString
int32BE n = BS.Internal.unsafeCreate 4 $ \ptr -> do
  poke ptr (fromIntegral (n `shiftR` 24) :: Word8)
  pokeByteOff ptr 1 (fromIntegral (n `shiftR` 16) :: Word8)
  pokeByteOff ptr 2 (fromIntegral (n `shiftR` 8) :: Word8)
  pokeByteOff ptr 3 (fromIntegral n :: Word8)
```

### Fused row collection and decoding

**Status:** `fetchAll` first collects all raw rows into a list, then
maps the decode function over them. This creates an intermediate
`[Vector (Maybe ByteString)]`.

**Implementation:** Fuse the collection and decode into a single pass:
decode each `DataRow` as it arrives from the wire, accumulating decoded
Haskell values directly.

### Pre-allocated receive buffer

**Status:** `recvExact` accumulates chunks in a list then `BS.concat`s
them. For large responses this creates many small allocations.

**Implementation:** Use a pre-allocated `MutableByteArray` or ring buffer
that grows as needed, avoiding repeated list construction.

### Unboxed vectors for fixed-size result columns

**Status:** Result rows use `Vector (Maybe ByteString)` — each value
is a boxed pointer to a heap-allocated `ByteString`.

**Implementation:** For columns of known fixed-size types (Int32, Bool,
etc.), use unboxed vectors to avoid per-value heap allocation.

---

## Ecosystem

### UUID binary codec

**Status:** UUID type (OID 2950) is mapped in the CLI TypeMap but has no
binary encode/decode instances in the runtime.

**Implementation:** UUID is 16 bytes, big-endian. Trivial to implement
using `uuid-types` (already a dependency).

### JSON/JSONB binary codec

**Status:** JSON (OID 114) and JSONB (OID 3802) are mapped in TypeMap
but have no binary encode/decode instances.

**Implementation:** JSON binary format is just the UTF-8 JSON text.
JSONB binary format is `\x01` version byte followed by UTF-8 JSON text.
Use `aeson` (already a dependency) for encode/decode.

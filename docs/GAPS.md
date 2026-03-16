# Known Gaps and Future Work

This document tracks features missing from the hsqlx wire protocol
implementation compared to libpq, as well as performance and ergonomic
improvements planned for future releases.

Last updated: 2026-03-16

---

## Completed

The following items were previously tracked as gaps and have since been
implemented:

- **TLS support** — SSLRequest subprotocol, TLS handshake via the `tls`
  library, TlsDisable/TlsPrefer/TlsRequire modes.
- **SCRAM channel binding** — `scramAuthWithChannelBinding` supports
  SCRAM-SHA-256-PLUS with `tls-server-end-point` binding.
- **Query cancellation** — `cancelQuery` opens a separate TCP connection
  and sends CancelRequest. `withQueryTimeout` for deadline-based
  cancellation.
- **Connection timeouts** — `ccConnectTimeout` in ConnConfig, parsed from
  connection strings (`connect_timeout=N`). `connectTcpTimeout` wraps
  `NS.connect` in `System.Timeout.timeout`.
- **Savepoints** — `withSavepoint` for nested partial rollbacks within
  transactions. Automatic ROLLBACK TO on exception, RELEASE on success.
- **Prepared statement eviction** — LRU cache capped at 256 per
  connection. Oldest statement closed on the server via Close message.
- **Parameterized cursors** — `withCursor` uses Parse/Bind/Execute to
  declare cursors with bound parameters. Queries with WHERE clauses
  work correctly with streaming.
- **UUID binary codec** — 16-byte encode/decode via `uuid-types`.
- **JSON/JSONB binary codec** — Handles both json (raw UTF-8) and jsonb
  (version byte prefix) binary formats via `aeson`.
- **INLINE pragmas** — All binary encode/decode instances, protocol
  builders/parsers, wire send/recv, and field encode/decode.
- **TCP_NODELAY** — Disabled Nagle's algorithm on all connections.
- **Message coalescing** — `sendFrontendMsgs` batches multiple protocol
  messages into a single `send()` syscall.
- **Pipelined batch execution** — `executeBatch` sends N Bind+Execute
  pairs with a single Sync, eliminating N-1 round-trips.
- **Connection health checking** — Pool validates idle connections with
  an empty query before reuse; dead connections are discarded.
- **Comprehensive PgError** — All 17 protocol error fields parsed.

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
`Builder -> LazyByteString -> toStrict`. For 4-8 bytes, this is
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

# Sender/Receiver Split Architecture

## Problem

hsqlx's `Connection` is currently serial: one thread sends a request and
blocks until the response arrives. When multiple green threads share a
connection (via pool), they serialize on the connection lock. This leaves
significant throughput on the table for concurrent workloads.

## Solution

Split each connection into three cooperating green threads after startup:

1. **Application threads** put `Request` into a `TBQueue`, block on `MVar`
   for the response.
2. **Writer thread** drains the `TBQueue`, batches messages into one
   `sendMany` syscall, enqueues response `MVar`s into a `TQueue`.
3. **Reader thread** reads from the socket, parses backend messages, fills
   `MVar`s in FIFO order.

**Key insight:** PostgreSQL processes messages in strict FIFO order. No
correlation IDs needed - the i-th response corresponds to the i-th pending
request. This is the same architecture behind asyncpg's 3x advantage over
psycopg2 on concurrent workloads.

## Current Architecture (Before)

```
App thread 1 ──┐
App thread 2 ──┼── serialize on Connection ── socket ── Postgres
App thread N ──┘
```

Each `fetchAll`/`execute` call directly calls `sendFrontendMsgs` and
`recvBackendMsg` on the `WireConn`. Multiple threads sharing a pooled
connection serialize on the connection.

## New Architecture (After)

```
App thread 1 ──┐                     ┌── Writer thread ── sendMany ──┐
App thread 2 ──┼── TBQueue(64) ──────┤                               ├── socket ── PG
App thread N ──┘                     └── Reader thread ── recvMsg  ──┘
                                              │
                                         TQueue (pending MVars, FIFO)
```

## Public API: Unchanged

All `fetchOne`, `fetchAll`, `execute`, `executeBatch`, `runPipeline`,
`withTransaction`, `copyIn`, `withCursor`, `executeWithFold`, etc. keep
their exact current signatures. The change is entirely internal.

---

## Implementation Plan

### Phase 1: New Wire Infrastructure

#### 1. Create `wire/src/PgWire/Async.hs` (NEW)

Core types and the async connection machinery.

**Request ADT** - what application threads submit:
```haskell
data Request
  = ReqExtendedQuery
      ![FrontendMsg]        -- Bind+Execute (Parse done separately)
      !ResponseCollector    -- how to collect the response
  | ReqSimpleQuery
      !ByteString           -- SQL text
  | ReqPrepare
      !FrontendMsg          -- Parse message
  | ReqExclusive
      !(WireConn -> IO ())  -- closure with direct socket access
```

**Response ADT** - what comes back via MVar:
```haskell
data Response
  = RespRows ![Vector (Maybe ByteString)]  -- collected DataRow vectors
  | RespCommand !CommandTag                 -- INSERT/UPDATE/DELETE result
  | RespSimple ![[Maybe ByteString]] !(Maybe CommandTag)
  | RespParsed                              -- ParseComplete acknowledged
  | RespBatchRows ![[Vector (Maybe ByteString)]]  -- one list per sub-query
```

**ResponseCollector** - tells the reader thread how to interpret backend
messages for this request:
```haskell
data ResponseCollector
  = CollectRows         -- collect DataRow until CommandComplete
  | CollectCommand      -- expect CommandComplete only
  | CollectParseComplete -- expect ParseComplete only
  | CollectBatch !Int   -- collect N sub-results (Bind+Execute repeated N times)
  | CollectSimple       -- simple query: text rows + optional tag
```

**PendingResponse** - enqueued by writer, dequeued by reader:
```haskell
data PendingResponse = PendingResponse
  { prCollector :: !ResponseCollector
  , prMVar      :: !(MVar (Either HsqlxError Response))
  }
```

**AsyncWireConn** - the async connection state:
```haskell
data AsyncWireConn = AsyncWireConn
  { awcWire          :: !WireConn
  , awcSendQueue     :: !(TBQueue (Request, MVar (Either HsqlxError Response)))
  , awcPending       :: !(TQueue PendingResponse)
  , awcAlive         :: !(TVar Bool)
  , awcTxStatus      :: !(IORef TxStatus)
  , awcNotifyHandler :: !(IORef (Notification -> IO ()))
  , awcNoticeHandler :: !(IORef (PgNotice -> IO ()))
  , awcParamStatus   :: !(IORef (Map ByteString ByteString))
  , awcWriterThread  :: !(Async ())
  , awcReaderThread  :: !(Async ())
  }
```

**`spawnAsyncWireConn`** - creates writer + reader after serial startup:
```haskell
spawnAsyncWireConn :: WireConn -> TxStatus -> Map ByteString ByteString
                   -> IO AsyncWireConn
```
- Creates TBQueue (capacity 64), TQueue, TVar, IORefs
- Spawns writer and reader via `async`
- Links them with `link2` so death of either kills both
- Returns the AsyncWireConn record

**`writerThread`** loop:
```haskell
writerThread :: AsyncWireConn -> IO ()
```
1. Block on `readTBQueue` for first request
2. Drain remaining available requests from queue (non-blocking `tryReadTBQueue`)
3. For each request:
   - Build the `[FrontendMsg]` list (adding Sync where needed)
   - Enqueue a `PendingResponse` into `awcPending`
4. Coalesce all messages and call `sendFrontendMsgs` once
5. Special case: `ReqExclusive` - flush all pending responses first (wait
   for reader to drain `awcPending`), then run the closure directly, then
   resume normal operation
6. Loop

**`readerThread`** loop:
```haskell
readerThread :: AsyncWireConn -> IO ()
```
1. Dequeue next `PendingResponse` from `awcPending`
2. Based on collector type, read backend messages:
   - `CollectRows`: accumulate `DataRow` until `CommandComplete`, then
     read `ReadyForQuery`, fill MVar with `RespRows`
   - `CollectCommand`: expect `CommandComplete` + `ReadyForQuery`, fill
     with `RespCommand`
   - `CollectParseComplete`: expect `ParseComplete`, fill with `RespParsed`
     (no ReadyForQuery - Parse doesn't trigger one without Sync)
   - `CollectBatch n`: collect n sub-results, fill with `RespBatchRows`
   - `CollectSimple`: simple query protocol collection
3. Inline dispatch for `NotificationResponse`, `NoticeResponse`,
   `ParameterStatus` - these can appear between any messages
4. Update `awcTxStatus` on `ReadyForQuery`
5. On error/exception: set `awcAlive` to False, fill ALL pending MVars
   with the error, then die

**`submitRequest`** - called by application threads:
```haskell
submitRequest :: AsyncWireConn -> Request -> ResponseCollector -> IO Response
```
1. Check `awcAlive` - if False, throw `ConnectionDead`
2. Create fresh `MVar`
3. Atomically write `(request, mvar)` to `awcSendQueue`
4. Block on `takeMVar` for response
5. Either return the `Response` or rethrow the `HsqlxError`

#### 2. Modify `wire/src/PgWire/Connection.hs`

Replace `connWire :: WireConn` with `connAsync :: AsyncWireConn`.

```haskell
data Connection = Connection
  { connAsync       :: !AsyncWireConn       -- was: connWire :: WireConn
  , connConfig      :: !ConnConfig
  , connBackendPid  :: {-# UNPACK #-} !Int32
  , connBackendKey  :: {-# UNPACK #-} !Int32
  , connStmtCache   :: !(IORef (Map ByteString ByteString))
  , connStmtCounter :: !(IORef Word64)
  , connSslActive   :: !Bool
  }
```

Add compatibility accessor:
```haskell
connWire :: Connection -> WireConn
connWire = awcWire . connAsync
```

Delegate to AsyncWireConn fields:
- `connTxStatus` -> `awcTxStatus . connAsync`
- `connParams` -> `awcParamStatus . connAsync`
- `connNoticeHandler` -> `awcNoticeHandler . connAsync`

**`connectSingleHost`**: serial startup (TCP, TLS, auth, parameter exchange)
stays exactly as-is, then calls `spawnAsyncWireConn` at the end before
returning the `Connection`.

**`close`**: signal shutdown (set `awcAlive` to False), cancel both async
threads, close the socket.

**`simpleQuery`**: submit `ReqSimpleQuery` via `submitRequest` instead of
direct wire calls.

**Status functions** (`connectionStatus`, `parameterStatus`, etc.): read
from AsyncWireConn IORefs (no wire traffic needed for most).

**`reset`**: close + reconnect (spawns new async threads).

#### 3. Add `ConnectionDead` to `wire/src/PgWire/Error.hs`

```haskell
data HsqlxError
  = ...existing constructors...
  | ConnectionDead         -- async threads have died
  deriving stock (Show, Eq)
```

---

### Phase 2: Runtime Module Updates (Parallel, Each Independent)

These modules currently call `sendFrontendMsgs`/`recvBackendMsg` directly.
They need to switch to `submitRequest`.

#### 4. `runtime/src/Hsqlx/Execute.hs`

The biggest change. Currently has:
- `ensurePrepared` - sends Parse, reads ParseComplete directly
- `executeExtended` - sends Bind+Execute+Sync via `sendFrontendMsgs`
- `collectRows` - reads DataRow messages in a loop
- `collectAndDecodeRows` - fused collect+decode
- `collectCommandResult` - reads CommandComplete
- `waitReady` - reads ReadyForQuery

After:
- `ensurePrepared` -> `ensurePreparedAsync`: submit `ReqPrepare` with
  `CollectParseComplete`, block, update cache
- `fetchOne`/`fetchAll`/`fetchScalar`: submit `ReqExtendedQuery` with
  `CollectRows`, decode from `RespRows`
- `execute`: submit with `CollectCommand`, extract count from `RespCommand`
- `executeBatch`/`fetchBatchOne`/`fetchBatchAll`: submit with
  `CollectBatch n`, decode from `RespBatchRows`
- Remove `collectRows`, `collectAndDecodeRows`, `collectCommandResult`,
  `waitReady`, `waitParseComplete` - this logic moves into `Async.hs`
  reader thread

#### 5. `runtime/src/Hsqlx/Pipeline.hs`

Currently: `runPipeline` collects all queries, prepares statements, sends
all Bind+Execute with one Sync, collects results in order.

After: prepare all stmts via async (each `ensurePreparedAsync` blocks
independently), then submit single `ReqExtendedQuery` with `CollectBatch`
containing all Bind+Execute messages.

#### 6. `runtime/src/Hsqlx/Copy.hs`

`copyIn`/`copyOut`/`copyInBinary` need direct socket access for the
streaming COPY protocol.

After: submit `ReqExclusive` with a closure that operates directly on
`WireConn`. The writer thread pauses the pipeline, waits for all pending
responses to drain, then hands control to the closure.

#### 7. `runtime/src/Hsqlx/Streaming.hs`

`withCursor` needs direct socket access for DECLARE/FETCH/CLOSE cursor
commands.

After: submit `ReqExclusive`, cursor scope runs with direct `WireConn`
access.

#### 8. `runtime/src/Hsqlx/Fold.hs`

`executeWithFold` needs to process rows one-at-a-time from the socket for
constant-memory operation.

After: submit `ReqExclusive` for constant-memory fold with direct socket
access. (Cannot buffer all rows in the reader thread - that defeats the
purpose of fold.)

#### 9. `runtime/src/Hsqlx/Batch.hs`

`fetchByIds`: simple extended query, same as fetchAll.

After: submit `ReqExtendedQuery` with `CollectRows`, same pattern as
fetchAll.

#### 10. `runtime/src/Hsqlx/Notify.hs`

Currently: `waitForNotification` sends empty query to flush, then polls
wire directly.

After: reader thread dispatches `NotificationResponse` to
`awcNotifyHandler` callback inline. `waitForNotification` registers a
callback that fills an MVar, then blocks on it. No more polling with
empty query.

#### 11. `runtime/src/Hsqlx/Transaction.hs`

Minimal change. `withTransaction` and `withSavepoint` use `simpleQuery`
for BEGIN/COMMIT/ROLLBACK/SAVEPOINT/RELEASE. Since `simpleQuery` now goes
through the async channel, this works automatically.

#### 12. `wire/src/PgWire/Pool.hs`

Minimal change. Health check currently calls `connectionStatus` which
sends an empty query. This now goes through `submitRequest` automatically.

---

## Key Design Decisions

### Startup stays serial
Async threads spawn only after authentication completes. The startup
handshake (SSL negotiation, SCRAM-SHA-256, parameter exchange) is
inherently serial and only happens once per connection.

### Statement prep is a separate round-trip
`ensurePreparedAsync` blocks before Bind is submitted. This means a
first-time query incurs two round-trips (Parse + Bind/Execute). Coalescing
Parse+Bind into a single submission is a future optimization that requires
speculative preparation.

### COPY/cursors/folds use exclusive mode
`ReqExclusive` pauses the pipeline, drains all pending responses, then
gives the closure direct `WireConn` access. This is necessary because:
- COPY protocol is a streaming state machine (CopyInResponse/CopyData/CopyDone)
- Cursors use FETCH which returns DataRow outside extended query protocol
- Folds need row-at-a-time processing (buffering defeats the purpose)

### TBQueue capacity 64 for backpressure
Prevents unbounded memory growth if application threads submit faster
than the wire can send. 64 is large enough to batch effectively but
small enough to apply backpressure before memory becomes a problem.

### FIFO TQueue for response demux
Writer enqueues PendingResponse in send order. Reader dequeues in same
order. PostgreSQL's strict FIFO guarantee means this is always correct.

### `link2` for thread death propagation
If the reader or writer dies (socket closed, parse error, etc.), both
threads die. All pending MVars are filled with `ConnectionDead` errors
so no application thread blocks forever.

### Error propagation
When the reader thread encounters a fatal error:
1. Set `awcAlive` to False
2. Drain `awcPending`, fill each MVar with `Left (ConnectionDead)`
3. Die (which kills writer via `link2`)

When the writer thread encounters a send error:
1. Same: set alive to False, die (reader sees EOF, fills MVars, dies)

Application threads that call `submitRequest` after death get an
immediate `ConnectionDead` exception from the alive check.

---

## Verification Plan

1. **All existing tests pass** - pg-wire-test, hsqlx-test, hsqlx-integration
   (48 tests). The public API is unchanged, so existing tests are the
   primary regression suite.

2. **New concurrent integration test** - 20 threads x 100 queries on the
   same connection (bypassing pool), all get correct results. Verifies
   FIFO demultiplexing under contention.

3. **Exclusive mode tests** - COPY, cursor, fold operations interleaved
   with concurrent queries. Verifies that exclusive mode correctly pauses
   and resumes the pipeline.

4. **Notification test** - verify async notification dispatch works without
   polling.

5. **Error propagation test** - kill the connection mid-query, verify all
   blocked threads get `ConnectionDead` promptly.

6. **Benchmark** - compare before/after on concurrent 32-thread workload
   to verify automatic pipelining delivers throughput improvement.

---

## File Change Summary

| File | Change |
|------|--------|
| `wire/src/PgWire/Async.hs` | **NEW** - core async machinery |
| `wire/src/PgWire/Connection.hs` | Replace `connWire` with `connAsync`, delegate status fields |
| `wire/src/PgWire/Error.hs` | Add `ConnectionDead` constructor |
| `wire/src/PgWire/Pool.hs` | Minimal - health check goes through channel |
| `wire/pg-wire.cabal` | Add `PgWire.Async` to exposed-modules, add `stm` dep |
| `runtime/src/Hsqlx/Execute.hs` | Replace direct wire calls with `submitRequest` |
| `runtime/src/Hsqlx/Pipeline.hs` | Use async prepare + batch submit |
| `runtime/src/Hsqlx/Copy.hs` | Use `ReqExclusive` |
| `runtime/src/Hsqlx/Streaming.hs` | Use `ReqExclusive` |
| `runtime/src/Hsqlx/Fold.hs` | Use `ReqExclusive` |
| `runtime/src/Hsqlx/Batch.hs` | Use `submitRequest` like fetchAll |
| `runtime/src/Hsqlx/Notify.hs` | Callback-based dispatch, no polling |
| `runtime/src/Hsqlx/Transaction.hs` | Minimal - simpleQuery goes through channel |

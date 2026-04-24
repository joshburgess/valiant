# Sender/Receiver Split Architecture

## Overview

Each `Connection` uses three cooperating green threads after startup:

1. **Application threads** put `Request` into a `TBQueue`, block on `MVar`
   for the response.
2. **Writer thread** drains the `TBQueue`, batches messages into one
   `sendMany` syscall, enqueues response `MVar`s into a `TQueue`.
3. **Reader thread** reads from the socket, parses backend messages, fills
   `MVar`s in FIFO order.

**Key insight:** PostgreSQL processes messages in strict FIFO order. No
correlation IDs needed. The i-th response corresponds to the i-th pending
request. This is the same architecture behind asyncpg's 3x advantage over
psycopg2 on concurrent workloads.

## Architecture

```
App thread 1 ──┐                     ┌── Writer thread ── send ────────┐
App thread 2 ──┼── TBQueue(64) ──────┤                                 ├── socket ── PG
App thread N ──┘                     └── Reader thread ── recvMsg  ────┘
                                              │
                                         TQueue (pending MVars, FIFO)
```

## Performance

### Concurrent throughput (single connection, `SELECT` by PK + `COUNT`):

| Threads | Total queries | Wall time | Queries/sec | Scaling |
|---------|--------------|-----------|-------------|---------|
| 1 | 100 | 124ms | 806/s | 1.0x |
| 4 | 400 | 212ms | 1,887/s | 2.3x |
| 16 | 1,600 | 420ms | 3,810/s | 4.7x |
| 32 | 3,200 | 553ms | 5,787/s | 7.2x |

32 threads on ONE connection achieve 7.2x the throughput of a single
thread. Without the async split, they would serialize and take 32x as
long.

## Implementation

### Core types (`wire/src/PgWire/Async.hs`)

**Request ADT**: what application threads submit:
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

**Response ADT**: what comes back via MVar:
```haskell
data Response
  = RespRows ![Vector (Maybe ByteString)]  -- collected DataRow vectors
  | RespCommand !CommandTag                 -- INSERT/UPDATE/DELETE result
  | RespSimple ![[Maybe ByteString]] !(Maybe CommandTag)
  | RespParsed                              -- ParseComplete acknowledged
  | RespBatchRows ![[Vector (Maybe ByteString)]]  -- one list per sub-query
```

**ResponseCollector**: tells the reader thread how to interpret backend
messages for this request:
```haskell
data ResponseCollector
  = CollectRows         -- collect DataRow until CommandComplete
  | CollectCommand      -- expect CommandComplete only
  | CollectParseComplete -- expect ParseComplete only
  | CollectBatch !Int   -- collect N sub-results
  | CollectSimple       -- simple query: text rows + optional tag
```

**AsyncWireConn**: the async connection state:
```haskell
data AsyncWireConn = AsyncWireConn
  { awcWire          :: !WireConn
  , awcSendQueue     :: !(TBQueue (Request, MVar (Either ValiantError Response)))
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

### Writer thread

1. Blocks on `readTBQueue` for first request
2. Drains remaining available requests (non-blocking `tryReadTBQueue`)
3. For each request: builds `[FrontendMsg]`, enqueues `PendingResponse`
4. Coalesces all messages into a single `send()` via `buildFrontendMsgsConcat`
5. Special case: `ReqExclusive` flushes all pending responses, runs
   closure with direct socket access, then resumes
6. Loops

### Reader thread

1. Dequeues next `PendingResponse` from `awcPending`
2. Based on collector type, reads backend messages:
   - `CollectRows`: accumulate `DataRow` until `CommandComplete`, then
     `ReadyForQuery`, fill MVar
   - `CollectCommand`: expect `CommandComplete` + `ReadyForQuery`
   - `CollectParseComplete`: expect `ParseComplete` (no ReadyForQuery)
   - `CollectBatch n`: collect n sub-results
   - `CollectSimple`: simple query protocol collection
3. Dispatches `NotificationResponse`, `NoticeResponse`, `ParameterStatus`
   inline (these can appear between any messages)
4. Updates `awcTxStatus` on `ReadyForQuery`
5. On error: sets `awcAlive` to False, fills ALL pending MVars with
   the error, then dies

### Error recovery

Query errors (`ErrorResponse`) are per-request, not connection-fatal.
The reader catches `ValiantError`, drains to `ReadyForQuery`, delivers the
error to the specific caller's MVar, and continues serving other requests.

Fatal errors (socket closed, parse failure) kill both threads. All
pending MVars are filled with `ConnectionDead`. Application threads that
call `submitRequest` after death get an immediate exception.

## Design Decisions

### Startup stays serial
Async threads spawn only after authentication completes. The startup
handshake (SSL negotiation, SCRAM-SHA-256, parameter exchange) is
inherently serial and only happens once per connection.

### Parse+Bind coalescing
First-time queries send Parse+Bind+Execute+Sync in a single round-trip.
Subsequent executions (cache hit) send Bind+Execute+Sync only.

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

## Module map

| File | Role |
|------|------|
| `wire/src/PgWire/Async.hs` | Core async machinery: types, writer, reader, submitRequest |
| `wire/src/PgWire/Connection.hs` | `Connection` wraps `AsyncWireConn`; spawns threads after startup |
| `wire/src/PgWire/Error.hs` | `ConnectionDead` error constructor |
| `runtime/src/Valiant/Execute.hs` | `submitRequest` with `CollectRows`/`CollectCommand` |
| `runtime/src/Valiant/Copy.hs` | `ReqExclusive` for COPY streaming |
| `runtime/src/Valiant/Streaming.hs` | `ReqExclusive` for cursor operations |
| `runtime/src/Valiant/Fold.hs` | `ReqExclusive` for constant-memory fold |
| `runtime/src/Valiant/Batch.hs` | `submitRequest` with `CollectRows` |
| `runtime/src/Valiant/Pipeline.hs` | `submitRequest` with `CollectBatch` |
| `runtime/src/Valiant/Notify.hs` | Callback-based notification dispatch |
| `runtime/src/Valiant/Transaction.hs` | `simpleQuery` goes through async channel |

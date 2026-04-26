# valiant-conduit

[Conduit](https://hackage.haskell.org/package/conduit) streaming
adapter for [`valiant`](https://hackage.haskell.org/package/valiant).

Produces `ConduitT () r IO ()` values from query results, so query
output composes with the rest of the conduit ecosystem.

## Quick start

```haskell
import Valiant
import Valiant.Conduit
import Conduit

countActiveUsers :: Pool -> IO Int
countActiveUsers pool =
  withTransaction pool $ \tx ->
    runConduit $
      selectSource (txConn tx) listAllUsers () 500
      .| filterC userActive
      .| lengthC
```

## Two streaming strategies

- `selectSource conn stmt params batchSize` — cursor-based. Must run
  inside a transaction. Fetches in batches of `batchSize` rows.
- `foldSource conn stmt params` — single-shot extended-protocol query.
  No transaction required.

Both functions accumulate one result set at a time before yielding,
so memory usage scales with the active batch (cursor) or the full
result set (`foldSource`). For truly incremental memory use across a
large cursor, drive `Valiant.withCursor` / `Valiant.fetchBatch`
yourself.

See the [valiant tutorial](https://github.com/joshburgess/valiant/blob/main/docs/TUTORIAL.md)
for `Statement` definitions and the `valiant prepare` workflow.

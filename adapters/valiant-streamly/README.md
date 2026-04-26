# valiant-streamly

[Streamly](https://hackage.haskell.org/package/streamly-core) adapter
for [`valiant`](https://hackage.haskell.org/package/valiant).

Produces `Stream IO r` values from query results.

## Quick start

```haskell
import Valiant
import Valiant.Streamly
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.Fold qualified as Fold

countUsers :: Pool -> IO Int
countUsers pool =
  withTransaction pool $ \tx ->
    Stream.fold Fold.length $
      selectStreamly (txConn tx) listAllUsers () 500
```

## Two streaming strategies

- `selectStreamly conn stmt params batchSize` — cursor-based. Must run
  inside a transaction.
- `foldStreamly conn stmt params` — single-shot extended-protocol
  query. No transaction required.

Both functions accumulate one result set at a time before yielding.
For truly incremental memory use, drive `Valiant.withCursor` /
`Valiant.fetchBatch` directly.

See the [valiant tutorial](https://github.com/joshburgess/valiant/blob/main/docs/TUTORIAL.md)
for `Statement` definitions and the `valiant prepare` workflow.

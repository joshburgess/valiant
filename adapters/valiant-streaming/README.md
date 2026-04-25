# valiant-streaming

[`streaming`](https://hackage.haskell.org/package/streaming) adapter
for [`valiant`](https://hackage.haskell.org/package/valiant).

Produces `Stream (Of r) IO ()` values from query results.

## Quick start

```haskell
import Valiant
import Valiant.Stream
import Streaming.Prelude qualified as S

countUsers :: Pool -> IO Int
countUsers pool =
  withTransaction pool $ \tx ->
    S.length_ $ selectStream (txConn tx) listAllUsers () 500
```

## Two streaming strategies

- `selectStream conn stmt params batchSize` — cursor-based. Must run
  inside a transaction.
- `foldStream conn stmt params` — single-shot extended-protocol query.
  No transaction required.

Both functions accumulate one result set at a time before yielding.
For truly incremental memory use, drive `Valiant.withCursor` /
`Valiant.fetchBatch` directly.

See the [valiant tutorial](https://github.com/joshburgess/valiant/blob/main/docs/tutorial.md)
for `Statement` definitions and the `valiant prepare` workflow.

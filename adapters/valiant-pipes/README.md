# valiant-pipes

[Pipes](https://hackage.haskell.org/package/pipes) streaming adapter
for [`valiant`](https://hackage.haskell.org/package/valiant).

Produces `Producer r IO ()` values from query results, so they compose
with the pipes ecosystem.

## Quick start

```haskell
import Valiant
import Valiant.Pipes
import Pipes
import Pipes.Prelude qualified as P

printAllUserNames :: Pool -> IO ()
printAllUserNames pool =
  withTransaction pool $ \tx ->
    runEffect $
      selectPipe (txConn tx) listAllUsers () 500
      >-> P.map userName
      >-> P.stdoutLn
```

## Two streaming strategies

- `selectPipe conn stmt params batchSize` — cursor-based. Must run
  inside a transaction.
- `foldPipe conn stmt params` — single-shot extended-protocol query.
  No transaction required.

Both functions accumulate one result set at a time before yielding,
so memory usage scales with the active batch (cursor) or the full
result set (`foldPipe`). For truly incremental memory use, drive
`Valiant.withCursor` / `Valiant.fetchBatch` directly.

See the [valiant tutorial](https://github.com/joshburgess/valiant/blob/main/docs/tutorial.md)
for `Statement` definitions and the `valiant prepare` workflow.

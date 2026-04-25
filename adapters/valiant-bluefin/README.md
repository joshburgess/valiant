# valiant-bluefin

[Bluefin](https://hackage.haskell.org/package/bluefin) effect adapter
for [`valiant`](https://hackage.haskell.org/package/valiant).

Bluefin uses explicit effect handles rather than type-level effect
lists. This adapter wraps a `Pool` as a `ValiantHandle e` that you
pass to functions that need database access.

## Quick start

```haskell
import Valiant.Bluefin
import PgWire.Pool (newPool, defaultPoolConfig, poolConnString)

myApp :: ValiantHandle e -> IO [User]
myApp db = fetchAllB db listUsers ()

main :: IO ()
main = do
  pool <- newPool defaultPoolConfig { poolConnString = "postgres://..." }
  result <- runValiantB pool myApp
  print result
```

## What you get

- `runValiantB :: Pool -> (forall e. ValiantHandle e -> IO a) -> IO a`
- Query operations: `fetchOneB`, `fetchAllB`, `fetchScalarB`,
  `fetchOneOrThrowB`, `fetchExistsB`
- Command operations: `executeB`, `executeReturningB`, `executeBatchB`
- Transactions: `withTransactionB`
- Raw access: `withConnectionB`

See the [valiant tutorial](https://github.com/joshburgess/valiant/blob/main/docs/tutorial.md)
for the underlying `Statement` and `valiant prepare` workflow.

# valiant-fused-effects

[fused-effects](https://hackage.haskell.org/package/fused-effects)
adapter for [`valiant`](https://hackage.haskell.org/package/valiant).

Provides a `Valiant` effect, a `ValiantPoolC` carrier (built on
`ReaderC Pool`), and `Has (Reader Pool) sig m`-constrained smart
constructors.

## Quick start

```haskell
import Control.Carrier.Lift (runM)
import Valiant.FusedEffects
import PgWire.Pool (newPool, defaultPoolConfig, poolConnString)

myApp :: (Has (Reader Pool) sig m, MonadIO m) => m [User]
myApp = do
  users <- fetchAllF listUsers ()
  pure users

main :: IO ()
main = do
  pool <- newPool defaultPoolConfig { poolConnString = "postgres://..." }
  users <- runM . runValiantPool pool $ myApp
  print users
```

## What you get

- Effect: `data Valiant`
- Carrier: `type ValiantPoolC m = ReaderC Pool m` with
  `runValiantPool :: Pool -> ValiantPoolC m a -> m a`
- Query operations: `fetchOneF`, `fetchAllF`, `fetchScalarF`,
  `fetchOneOrThrowF`, `fetchExistsF`
- Command operations: `executeF`, `executeReturningF`, `executeBatchF`
- Transactions: `withTransactionF`
- Raw access: `withConnectionF`

See the [valiant tutorial](https://github.com/joshburgess/valiant/blob/main/docs/tutorial.md)
for `Statement` definitions and the `valiant prepare` workflow.

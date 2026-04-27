# valiant-effectful

[Effectful](https://hackage.haskell.org/package/effectful) adapter for
[`valiant`](https://hackage.haskell.org/package/valiant).

Provides a dynamic `Valiant` effect that any `Eff` action can request
via `Valiant :> es`, plus a pool-based handler.

## Quick start

```haskell
import Effectful
import Valiant (newPool, defaultPoolConfig, poolConnString)
import Valiant.Effectful

myApp :: (Valiant :> es, IOE :> es) => Eff es [User]
myApp = do
  users <- fetchAllEff listUsers ()
  count <- fetchScalarEff countUsers ()
  liftIO $ putStrLn $ "got " <> show count <> " users"
  pure users

main :: IO ()
main = do
  pool <- newPool defaultPoolConfig { poolConnString = "postgres://..." }
  users <- runEff . runValiant pool $ myApp
  print users
```

## What you get

- Effect: `data Valiant :: Effect`
- Handlers: `runValiant pool` and `runValiantWith` (custom handler,
  useful for tests)
- Query operations: `fetchOneEff`, `fetchAllEff`, `fetchScalarEff`,
  `fetchOneOrThrowEff`, `fetchExistsEff`
- Command operations: `executeEff`, `executeReturningEff`,
  `executeBatchEff`
- Transactions: `withTransactionEff`, `withTransactionLevelEff`
- Raw access: `withConnectionEff`

See the [valiant tutorial](https://github.com/joshburgess/valiant/blob/main/docs/TUTORIAL.md)
for the underlying `Statement` and `valiant prepare` workflow.

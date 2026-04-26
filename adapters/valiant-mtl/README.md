# valiant-mtl

MTL-style adapter for
[`valiant`](https://hackage.haskell.org/package/valiant).

All operations work in any monad with `HasPool m` (a `MonadReader`-like
class that exposes a `Pool`) and `MonadIO m`. Use this when your
application lives in an `mtl` stack with a richer environment than
just `Pool`.

## Quick start

```haskell
import Control.Monad.Reader
import Valiant.Mtl
import PgWire.Pool (newPool, defaultPoolConfig, poolConnString)

data AppEnv = AppEnv
  { appPool   :: Pool
  , appLogger :: Logger
  }

instance HasPool (ReaderT AppEnv IO) where
  getPool = asks appPool

myApp :: (HasPool m, MonadIO m) => m [User]
myApp = do
  users <- fetchAllMtl listUsers ()
  count <- fetchScalarMtl countUsers ()
  pure users

main :: IO ()
main = do
  pool   <- newPool defaultPoolConfig { poolConnString = "postgres://..." }
  logger <- newLogger
  runReaderT myApp (AppEnv pool logger)
```

## What you get

- `class HasPool m where getPool :: m Pool`
- Query operations: `fetchOneMtl`, `fetchAllMtl`, `fetchScalarMtl`,
  `fetchOneOrThrowMtl`, `fetchExistsMtl`, `forEachMtl`
- Command operations: `executeMtl`, `executeReturningMtl`,
  `executeBatchMtl`
- Transactions: `withTransactionMtl`, `withTransactionLevelMtl`
- Pool helpers: `poolStatsMtl`, `withConnectionMtl`

There is also a `Valiant.Monad` module re-exported from this package
that provides the same operations specialised to
`ReaderT Pool IO`.

See the [valiant tutorial](https://github.com/joshburgess/valiant/blob/main/docs/TUTORIAL.md)
for `Statement` definitions and the `valiant prepare` workflow.

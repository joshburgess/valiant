-- | Effectful adapter for hsqlx.
--
-- Provides an @Hsqlx@ effect for database operations, with a pool-based
-- handler that manages connection acquisition and release automatically.
--
-- @
-- import Effectful
-- import Hsqlx.Effectful
--
-- myApp :: (Hsqlx :> es, IOE :> es) => Eff es [User]
-- myApp = do
--   users <- fetchAllEff listUsers ()
--   count <- fetchScalarEff countUsers ()
--   pure users
--
-- main :: IO ()
-- main = do
--   pool <- newPool defaultPoolConfig { poolConnString = "..." }
--   result <- runEff . runHsqlx pool $ myApp
--   print result
-- @
module Hsqlx.Effectful
  ( -- * Effect
    Hsqlx (..)

    -- * Handler
  , runHsqlx
  , runHsqlxWith

    -- * Query operations
  , fetchOneEff
  , fetchAllEff
  , fetchScalarEff
  , fetchOneOrThrowEff
  , fetchExistsEff

    -- * Command operations
  , executeEff
  , executeReturningEff
  , executeBatchEff

    -- * Transaction operations
  , withTransactionEff
  , withTransactionLevelEff

    -- * Raw access
  , withConnectionEff
  ) where

import Data.Int (Int64)
import Effectful
import Effectful.Dispatch.Dynamic
import Hsqlx (Connection, IsolationLevel, Pool, Statement, Transaction (..))
import Hsqlx qualified
import PgWire.Pool (withResource)

------------------------------------------------------------------------
-- Effect definition
------------------------------------------------------------------------

-- | The Hsqlx database effect.
--
-- All operations acquire a connection from the pool, execute, and release.
-- Transactions hold a connection for the duration of the callback.
data Hsqlx :: Effect where
  FetchOneEff :: Statement p r -> p -> Hsqlx m (Maybe r)
  FetchAllEff :: Statement p r -> p -> Hsqlx m [r]
  FetchScalarEff :: Statement p r -> p -> Hsqlx m r
  FetchOneOrThrowEff :: Statement p r -> p -> Hsqlx m r
  FetchExistsEff :: Statement p r -> p -> Hsqlx m Bool
  ExecuteEff :: Statement p () -> p -> Hsqlx m Int64
  ExecuteReturningEff :: Statement p r -> p -> Hsqlx m (Int64, [r])
  ExecuteBatchEff :: Statement p () -> [p] -> Hsqlx m Int64
  WithTransactionEff :: (Transaction -> IO a) -> Hsqlx m a
  WithTransactionLevelEff :: IsolationLevel -> (Transaction -> IO a) -> Hsqlx m a
  WithConnectionEff :: (Connection -> IO a) -> Hsqlx m a

type instance DispatchOf Hsqlx = 'Dynamic

------------------------------------------------------------------------
-- Handler
------------------------------------------------------------------------

-- | Run the Hsqlx effect with a connection pool.
--
-- @
-- main = do
--   pool <- newPool defaultPoolConfig { poolConnString = "..." }
--   runEff . runHsqlx pool $ do
--     users <- fetchAllEff listUsers ()
--     liftIO $ print users
-- @
runHsqlx :: (IOE :> es) => Pool -> Eff (Hsqlx : es) a -> Eff es a
runHsqlx pool = interpret $ \_ -> \case
  FetchOneEff stmt params ->
    liftIO $ withResource pool $ \conn -> Hsqlx.fetchOne conn stmt params
  FetchAllEff stmt params ->
    liftIO $ withResource pool $ \conn -> Hsqlx.fetchAll conn stmt params
  FetchScalarEff stmt params ->
    liftIO $ withResource pool $ \conn -> Hsqlx.fetchScalar conn stmt params
  FetchOneOrThrowEff stmt params ->
    liftIO $ withResource pool $ \conn -> Hsqlx.fetchOneOrThrow conn stmt params
  FetchExistsEff stmt params ->
    liftIO $ withResource pool $ \conn -> Hsqlx.fetchExists conn stmt params
  ExecuteEff stmt params ->
    liftIO $ withResource pool $ \conn -> Hsqlx.execute conn stmt params
  ExecuteReturningEff stmt params ->
    liftIO $ withResource pool $ \conn -> Hsqlx.executeReturning conn stmt params
  ExecuteBatchEff stmt paramsList ->
    liftIO $ withResource pool $ \conn -> Hsqlx.executeBatch conn stmt paramsList
  WithTransactionEff action ->
    liftIO $ Hsqlx.withTransaction pool action
  WithTransactionLevelEff level action ->
    liftIO $ Hsqlx.withTransactionLevel level pool action
  WithConnectionEff action ->
    liftIO $ withResource pool action

-- | Run the Hsqlx effect with a custom handler function.
-- Useful for testing with mock connections.
runHsqlxWith
  :: (IOE :> es)
  => (forall x. (Connection -> IO x) -> IO x)
  -- ^ Connection provider (e.g., 'withResource pool' or a mock)
  -> (forall x. (Transaction -> IO x) -> IO x)
  -- ^ Transaction provider
  -> Eff (Hsqlx : es) a
  -> Eff es a
runHsqlxWith withConn withTxn = interpret $ \_ -> \case
  FetchOneEff stmt params ->
    liftIO $ withConn $ \conn -> Hsqlx.fetchOne conn stmt params
  FetchAllEff stmt params ->
    liftIO $ withConn $ \conn -> Hsqlx.fetchAll conn stmt params
  FetchScalarEff stmt params ->
    liftIO $ withConn $ \conn -> Hsqlx.fetchScalar conn stmt params
  FetchOneOrThrowEff stmt params ->
    liftIO $ withConn $ \conn -> Hsqlx.fetchOneOrThrow conn stmt params
  FetchExistsEff stmt params ->
    liftIO $ withConn $ \conn -> Hsqlx.fetchExists conn stmt params
  ExecuteEff stmt params ->
    liftIO $ withConn $ \conn -> Hsqlx.execute conn stmt params
  ExecuteReturningEff stmt params ->
    liftIO $ withConn $ \conn -> Hsqlx.executeReturning conn stmt params
  ExecuteBatchEff stmt paramsList ->
    liftIO $ withConn $ \conn -> Hsqlx.executeBatch conn stmt paramsList
  WithTransactionEff action ->
    liftIO $ withTxn action
  WithTransactionLevelEff _level action ->
    liftIO $ withTxn action  -- level ignored in custom handler
  WithConnectionEff action ->
    liftIO $ withConn action

------------------------------------------------------------------------
-- Smart constructors
------------------------------------------------------------------------

-- | Fetch zero or one row.
fetchOneEff :: (Hsqlx :> es) => Statement p r -> p -> Eff es (Maybe r)
fetchOneEff stmt params = send (FetchOneEff stmt params)

-- | Fetch all rows.
fetchAllEff :: (Hsqlx :> es) => Statement p r -> p -> Eff es [r]
fetchAllEff stmt params = send (FetchAllEff stmt params)

-- | Fetch exactly one scalar value. Throws on zero or multiple rows.
fetchScalarEff :: (Hsqlx :> es) => Statement p r -> p -> Eff es r
fetchScalarEff stmt params = send (FetchScalarEff stmt params)

-- | Fetch one row, throwing if none returned.
fetchOneOrThrowEff :: (Hsqlx :> es) => Statement p r -> p -> Eff es r
fetchOneOrThrowEff stmt params = send (FetchOneOrThrowEff stmt params)

-- | Check if a query returns any rows.
fetchExistsEff :: (Hsqlx :> es) => Statement p r -> p -> Eff es Bool
fetchExistsEff stmt params = send (FetchExistsEff stmt params)

-- | Execute a command. Returns rows affected.
executeEff :: (Hsqlx :> es) => Statement p () -> p -> Eff es Int64
executeEff stmt params = send (ExecuteEff stmt params)

-- | Execute with RETURNING. Returns rows affected and decoded rows.
executeReturningEff :: (Hsqlx :> es) => Statement p r -> p -> Eff es (Int64, [r])
executeReturningEff stmt params = send (ExecuteReturningEff stmt params)

-- | Execute a batch of commands (pipelined). Returns total rows affected.
executeBatchEff :: (Hsqlx :> es) => Statement p () -> [p] -> Eff es Int64
executeBatchEff stmt paramsList = send (ExecuteBatchEff stmt paramsList)

-- | Run an action in a transaction.
withTransactionEff :: (Hsqlx :> es) => (Transaction -> IO a) -> Eff es a
withTransactionEff action = send (WithTransactionEff action)

-- | Run an action in a transaction with a specific isolation level.
withTransactionLevelEff :: (Hsqlx :> es) => IsolationLevel -> (Transaction -> IO a) -> Eff es a
withTransactionLevelEff level action = send (WithTransactionLevelEff level action)

-- | Run an action with a raw connection from the pool.
withConnectionEff :: (Hsqlx :> es) => (Connection -> IO a) -> Eff es a
withConnectionEff action = send (WithConnectionEff action)

-- | Effectful adapter for valiant.
--
-- Provides an @Valiant@ effect for database operations, with a pool-based
-- handler that manages connection acquisition and release automatically.
--
-- @
-- import Effectful
-- import Valiant.Effectful
--
-- myApp :: (Valiant :> es, IOE :> es) => Eff es [User]
-- myApp = do
--   users <- fetchAllEff listUsers ()
--   count <- fetchScalarEff countUsers ()
--   pure users
--
-- main :: IO ()
-- main = do
--   pool <- newPool defaultPoolConfig { poolConnString = "..." }
--   result <- runEff . runValiant pool $ myApp
--   print result
-- @
module Valiant.Effectful
  ( -- * Effect
    Valiant (..)

    -- * Handler
  , runValiant
  , runValiantWith

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
import Valiant (Connection, IsolationLevel, Pool, Statement, Transaction (..))
import Valiant qualified
import PgWire.Pool (withResource)

------------------------------------------------------------------------
-- Effect definition
------------------------------------------------------------------------

-- | The Valiant database effect.
--
-- All operations acquire a connection from the pool, execute, and release.
-- Transactions hold a connection for the duration of the callback.
data Valiant :: Effect where
  FetchOneEff :: Statement p r -> p -> Valiant m (Maybe r)
  FetchAllEff :: Statement p r -> p -> Valiant m [r]
  FetchScalarEff :: Statement p r -> p -> Valiant m r
  FetchOneOrThrowEff :: Statement p r -> p -> Valiant m r
  FetchExistsEff :: Statement p r -> p -> Valiant m Bool
  ExecuteEff :: Statement p () -> p -> Valiant m Int64
  ExecuteReturningEff :: Statement p r -> p -> Valiant m (Int64, [r])
  ExecuteBatchEff :: Statement p () -> [p] -> Valiant m Int64
  WithTransactionEff :: (Transaction -> IO a) -> Valiant m a
  WithTransactionLevelEff :: IsolationLevel -> (Transaction -> IO a) -> Valiant m a
  WithConnectionEff :: (Connection -> IO a) -> Valiant m a

type instance DispatchOf Valiant = 'Dynamic

------------------------------------------------------------------------
-- Handler
------------------------------------------------------------------------

-- | Run the Valiant effect with a connection pool.
--
-- @
-- main = do
--   pool <- newPool defaultPoolConfig { poolConnString = "..." }
--   runEff . runValiant pool $ do
--     users <- fetchAllEff listUsers ()
--     liftIO $ print users
-- @
runValiant :: (IOE :> es) => Pool -> Eff (Valiant : es) a -> Eff es a
runValiant pool = interpret $ \_ -> \case
  FetchOneEff stmt params ->
    liftIO $ withResource pool $ \conn -> Valiant.fetchOne conn stmt params
  FetchAllEff stmt params ->
    liftIO $ withResource pool $ \conn -> Valiant.fetchAll conn stmt params
  FetchScalarEff stmt params ->
    liftIO $ withResource pool $ \conn -> Valiant.fetchScalar conn stmt params
  FetchOneOrThrowEff stmt params ->
    liftIO $ withResource pool $ \conn -> Valiant.fetchOneOrThrow conn stmt params
  FetchExistsEff stmt params ->
    liftIO $ withResource pool $ \conn -> Valiant.fetchExists conn stmt params
  ExecuteEff stmt params ->
    liftIO $ withResource pool $ \conn -> Valiant.execute conn stmt params
  ExecuteReturningEff stmt params ->
    liftIO $ withResource pool $ \conn -> Valiant.executeReturning conn stmt params
  ExecuteBatchEff stmt paramsList ->
    liftIO $ withResource pool $ \conn -> Valiant.executeBatch conn stmt paramsList
  WithTransactionEff action ->
    liftIO $ Valiant.withTransaction pool action
  WithTransactionLevelEff level action ->
    liftIO $ Valiant.withTransactionLevel level pool action
  WithConnectionEff action ->
    liftIO $ withResource pool action

-- | Run the Valiant effect with a custom handler function.
-- Useful for testing with mock connections.
runValiantWith
  :: (IOE :> es)
  => (forall x. (Connection -> IO x) -> IO x)
  -- ^ Connection provider (e.g., 'withResource pool' or a mock)
  -> (forall x. (Transaction -> IO x) -> IO x)
  -- ^ Transaction provider
  -> Eff (Valiant : es) a
  -> Eff es a
runValiantWith withConn withTxn = interpret $ \_ -> \case
  FetchOneEff stmt params ->
    liftIO $ withConn $ \conn -> Valiant.fetchOne conn stmt params
  FetchAllEff stmt params ->
    liftIO $ withConn $ \conn -> Valiant.fetchAll conn stmt params
  FetchScalarEff stmt params ->
    liftIO $ withConn $ \conn -> Valiant.fetchScalar conn stmt params
  FetchOneOrThrowEff stmt params ->
    liftIO $ withConn $ \conn -> Valiant.fetchOneOrThrow conn stmt params
  FetchExistsEff stmt params ->
    liftIO $ withConn $ \conn -> Valiant.fetchExists conn stmt params
  ExecuteEff stmt params ->
    liftIO $ withConn $ \conn -> Valiant.execute conn stmt params
  ExecuteReturningEff stmt params ->
    liftIO $ withConn $ \conn -> Valiant.executeReturning conn stmt params
  ExecuteBatchEff stmt paramsList ->
    liftIO $ withConn $ \conn -> Valiant.executeBatch conn stmt paramsList
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
fetchOneEff :: (Valiant :> es) => Statement p r -> p -> Eff es (Maybe r)
fetchOneEff stmt params = send (FetchOneEff stmt params)

-- | Fetch all rows.
fetchAllEff :: (Valiant :> es) => Statement p r -> p -> Eff es [r]
fetchAllEff stmt params = send (FetchAllEff stmt params)

-- | Fetch exactly one scalar value. Throws on zero or multiple rows.
fetchScalarEff :: (Valiant :> es) => Statement p r -> p -> Eff es r
fetchScalarEff stmt params = send (FetchScalarEff stmt params)

-- | Fetch one row, throwing if none returned.
fetchOneOrThrowEff :: (Valiant :> es) => Statement p r -> p -> Eff es r
fetchOneOrThrowEff stmt params = send (FetchOneOrThrowEff stmt params)

-- | Check if a query returns any rows.
fetchExistsEff :: (Valiant :> es) => Statement p r -> p -> Eff es Bool
fetchExistsEff stmt params = send (FetchExistsEff stmt params)

-- | Execute a command. Returns rows affected.
executeEff :: (Valiant :> es) => Statement p () -> p -> Eff es Int64
executeEff stmt params = send (ExecuteEff stmt params)

-- | Execute with RETURNING. Returns rows affected and decoded rows.
executeReturningEff :: (Valiant :> es) => Statement p r -> p -> Eff es (Int64, [r])
executeReturningEff stmt params = send (ExecuteReturningEff stmt params)

-- | Execute a batch of commands (pipelined). Returns total rows affected.
executeBatchEff :: (Valiant :> es) => Statement p () -> [p] -> Eff es Int64
executeBatchEff stmt paramsList = send (ExecuteBatchEff stmt paramsList)

-- | Run an action in a transaction.
withTransactionEff :: (Valiant :> es) => (Transaction -> IO a) -> Eff es a
withTransactionEff action = send (WithTransactionEff action)

-- | Run an action in a transaction with a specific isolation level.
withTransactionLevelEff :: (Valiant :> es) => IsolationLevel -> (Transaction -> IO a) -> Eff es a
withTransactionLevelEff level action = send (WithTransactionLevelEff level action)

-- | Run an action with a raw connection from the pool.
withConnectionEff :: (Valiant :> es) => (Connection -> IO a) -> Eff es a
withConnectionEff action = send (WithConnectionEff action)

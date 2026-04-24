-- | MTL-style adapter for valiant.
--
-- All operations work in any monad with @HasPool m@ (which is
-- @MonadReader env m@ where @env@ has a 'Pool') and @MonadIO m@.
-- This allows valiant to compose with arbitrary monad stacks.
--
-- @
-- data AppEnv = AppEnv { appPool :: Pool, appLogger :: Logger }
--
-- instance HasPool (ReaderT AppEnv IO) where
--   getPool = asks appPool
--
-- myApp :: (HasPool m, MonadIO m) => m [User]
-- myApp = do
--   users <- fetchAllMtl listUsers ()
--   count <- fetchScalarMtl countUsers ()
--   pure users
-- @
module Valiant.Mtl
  ( -- * Pool access
    HasPool (..)

    -- * Query operations
  , fetchOneMtl
  , fetchAllMtl
  , fetchScalarMtl
  , fetchOneOrThrowMtl
  , fetchExistsMtl
  , forEachMtl

    -- * Command operations
  , executeMtl
  , executeReturningMtl
  , executeBatchMtl

    -- * Transaction operations
  , withTransactionMtl
  , withTransactionLevelMtl

    -- * Pool management
  , poolStatsMtl
  , withConnectionMtl
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int64)
import Valiant (Connection, IsolationLevel, Pool, PoolStats, Statement, Transaction)
import Valiant qualified
import PgWire.Pool (poolStats, withResource)

-- | Type class for monads that can provide a 'Pool'.
--
-- Implement this for your application monad:
--
-- @
-- instance HasPool (ReaderT Pool IO) where
--   getPool = ask
--
-- -- Or with a larger environment:
-- instance HasPool (ReaderT AppEnv IO) where
--   getPool = asks appPool
-- @
class Monad m => HasPool m where
  getPool :: m Pool

-- | Fetch zero or one row.
fetchOneMtl :: (HasPool m, MonadIO m) => Statement p r -> p -> m (Maybe r)
fetchOneMtl stmt params = do
  pool <- getPool
  liftIO $ withResource pool $ \conn -> Valiant.fetchOne conn stmt params

-- | Fetch all rows.
fetchAllMtl :: (HasPool m, MonadIO m) => Statement p r -> p -> m [r]
fetchAllMtl stmt params = do
  pool <- getPool
  liftIO $ withResource pool $ \conn -> Valiant.fetchAll conn stmt params

-- | Fetch exactly one scalar value.
fetchScalarMtl :: (HasPool m, MonadIO m) => Statement p r -> p -> m r
fetchScalarMtl stmt params = do
  pool <- getPool
  liftIO $ withResource pool $ \conn -> Valiant.fetchScalar conn stmt params

-- | Fetch one row, throwing if none returned.
fetchOneOrThrowMtl :: (HasPool m, MonadIO m) => Statement p r -> p -> m r
fetchOneOrThrowMtl stmt params = do
  pool <- getPool
  liftIO $ withResource pool $ \conn -> Valiant.fetchOneOrThrow conn stmt params

-- | Check if a query returns any rows.
fetchExistsMtl :: (HasPool m, MonadIO m) => Statement p r -> p -> m Bool
fetchExistsMtl stmt params = do
  pool <- getPool
  liftIO $ withResource pool $ \conn -> Valiant.fetchExists conn stmt params

-- | Execute a callback for each row (streaming, no intermediate list).
forEachMtl :: (HasPool m, MonadIO m) => Statement p r -> p -> (r -> IO ()) -> m ()
forEachMtl stmt params action = do
  pool <- getPool
  liftIO $ withResource pool $ \conn -> Valiant.forEach conn stmt params action

-- | Execute a command. Returns rows affected.
executeMtl :: (HasPool m, MonadIO m) => Statement p () -> p -> m Int64
executeMtl stmt params = do
  pool <- getPool
  liftIO $ withResource pool $ \conn -> Valiant.execute conn stmt params

-- | Execute with RETURNING.
executeReturningMtl :: (HasPool m, MonadIO m) => Statement p r -> p -> m (Int64, [r])
executeReturningMtl stmt params = do
  pool <- getPool
  liftIO $ withResource pool $ \conn -> Valiant.executeReturning conn stmt params

-- | Execute a batch (pipelined). Returns total rows affected.
executeBatchMtl :: (HasPool m, MonadIO m) => Statement p () -> [p] -> m Int64
executeBatchMtl stmt paramsList = do
  pool <- getPool
  liftIO $ withResource pool $ \conn -> Valiant.executeBatch conn stmt paramsList

-- | Run an action in a transaction.
withTransactionMtl :: (HasPool m, MonadIO m) => (Transaction -> IO a) -> m a
withTransactionMtl action = do
  pool <- getPool
  liftIO $ Valiant.withTransaction pool action

-- | Run an action in a transaction with a specific isolation level.
withTransactionLevelMtl :: (HasPool m, MonadIO m) => IsolationLevel -> (Transaction -> IO a) -> m a
withTransactionLevelMtl level action = do
  pool <- getPool
  liftIO $ Valiant.withTransactionLevel level pool action

-- | Get pool statistics.
poolStatsMtl :: (HasPool m, MonadIO m) => m PoolStats
poolStatsMtl = do
  pool <- getPool
  liftIO $ poolStats pool

-- | Run an action with a raw connection.
withConnectionMtl :: (HasPool m, MonadIO m) => (Connection -> IO a) -> m a
withConnectionMtl action = do
  pool <- getPool
  liftIO $ withResource pool action

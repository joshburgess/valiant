{-# LANGUAGE RankNTypes #-}

-- | Fused-effects adapter for hsqlx.
--
-- Provides an @Hsqlx@ effect with a pool-based carrier via 'ReaderC'.
--
-- @
-- import Control.Carrier.Lift (runM)
-- import Hsqlx.FusedEffects
--
-- myApp :: Has Hsqlx sig m => m [User]
-- myApp = do
--   users <- fetchAllF listUsers ()
--   pure users
--
-- main :: IO ()
-- main = do
--   pool <- newPool defaultPoolConfig { poolConnString = "..." }
--   result <- runM . runHsqlxPool pool $ myApp
--   print result
-- @
module Hsqlx.FusedEffects
  ( -- * Effect
    Hsqlx (..)

    -- * Carrier
  , HsqlxPoolC
  , runHsqlxPool

    -- * Query operations
  , fetchOneF
  , fetchAllF
  , fetchScalarF
  , fetchOneOrThrowF
  , fetchExistsF

    -- * Command operations
  , executeF
  , executeReturningF
  , executeBatchF

    -- * Transaction operations
  , withTransactionF

    -- * Raw access
  , withConnectionF
  ) where

import Control.Algebra
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Effect.Reader (Reader, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int64)
import Data.Kind (Type)
import Hsqlx (Connection, Pool, Statement, Transaction)
import Hsqlx qualified
import PgWire.Pool (withResource)

------------------------------------------------------------------------
-- Effect
------------------------------------------------------------------------

-- | The Hsqlx database effect for fused-effects.
data Hsqlx (m :: Type -> Type) k where
  FetchOneF :: Statement p r -> p -> Hsqlx m (Maybe r)
  FetchAllF :: Statement p r -> p -> Hsqlx m [r]
  FetchScalarF :: Statement p r -> p -> Hsqlx m r
  FetchOneOrThrowF :: Statement p r -> p -> Hsqlx m r
  FetchExistsF :: Statement p r -> p -> Hsqlx m Bool
  ExecuteF :: Statement p () -> p -> Hsqlx m Int64
  ExecuteReturningF :: Statement p r -> p -> Hsqlx m (Int64, [r])
  ExecuteBatchF :: Statement p () -> [p] -> Hsqlx m Int64
  WithTransactionF :: (Transaction -> IO a) -> Hsqlx m a
  WithConnectionF :: (Connection -> IO a) -> Hsqlx m a

------------------------------------------------------------------------
-- Carrier (via ReaderC Pool)
------------------------------------------------------------------------

-- | Pool-based carrier. Uses 'ReaderC Pool' internally.
type HsqlxPoolC m = ReaderC Pool m

-- | Run the Hsqlx effect with a connection pool.
runHsqlxPool :: Pool -> HsqlxPoolC m a -> m a
runHsqlxPool = runReader

------------------------------------------------------------------------
-- Smart constructors (interpret directly via reader + IO)
------------------------------------------------------------------------

-- | Fetch zero or one row.
fetchOneF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p r -> p -> m (Maybe r)
fetchOneF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Hsqlx.fetchOne conn stmt params

-- | Fetch all rows.
fetchAllF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p r -> p -> m [r]
fetchAllF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Hsqlx.fetchAll conn stmt params

-- | Fetch exactly one scalar value.
fetchScalarF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p r -> p -> m r
fetchScalarF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Hsqlx.fetchScalar conn stmt params

-- | Fetch one row, throwing if none returned.
fetchOneOrThrowF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p r -> p -> m r
fetchOneOrThrowF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Hsqlx.fetchOneOrThrow conn stmt params

-- | Check if a query returns any rows.
fetchExistsF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p r -> p -> m Bool
fetchExistsF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Hsqlx.fetchExists conn stmt params

-- | Execute a command. Returns rows affected.
executeF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p () -> p -> m Int64
executeF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Hsqlx.execute conn stmt params

-- | Execute with RETURNING.
executeReturningF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p r -> p -> m (Int64, [r])
executeReturningF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Hsqlx.executeReturning conn stmt params

-- | Execute a batch of commands (pipelined).
executeBatchF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p () -> [p] -> m Int64
executeBatchF stmt paramsList = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Hsqlx.executeBatch conn stmt paramsList

-- | Run an action in a transaction.
withTransactionF :: (Has (Reader Pool) sig m, MonadIO m) => (Transaction -> IO a) -> m a
withTransactionF action = do
  pool <- ask
  liftIO $ Hsqlx.withTransaction pool action

-- | Run an action with a raw connection.
withConnectionF :: (Has (Reader Pool) sig m, MonadIO m) => (Connection -> IO a) -> m a
withConnectionF action = do
  pool <- ask
  liftIO $ withResource pool action

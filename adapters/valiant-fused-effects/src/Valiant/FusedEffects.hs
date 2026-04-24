{-# LANGUAGE RankNTypes #-}

-- | Fused-effects adapter for valiant.
--
-- Provides an @Valiant@ effect with a pool-based carrier via 'ReaderC'.
--
-- @
-- import Control.Carrier.Lift (runM)
-- import Valiant.FusedEffects
--
-- myApp :: Has Valiant sig m => m [User]
-- myApp = do
--   users <- fetchAllF listUsers ()
--   pure users
--
-- main :: IO ()
-- main = do
--   pool <- newPool defaultPoolConfig { poolConnString = "..." }
--   result <- runM . runValiantPool pool $ myApp
--   print result
-- @
module Valiant.FusedEffects
  ( -- * Effect
    Valiant (..)

    -- * Carrier
  , ValiantPoolC
  , runValiantPool

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
import Valiant (Connection, Pool, Statement, Transaction)
import Valiant qualified
import PgWire.Pool (withResource)

------------------------------------------------------------------------
-- Effect
------------------------------------------------------------------------

-- | The Valiant database effect for fused-effects.
data Valiant (m :: Type -> Type) k where
  FetchOneF :: Statement p r -> p -> Valiant m (Maybe r)
  FetchAllF :: Statement p r -> p -> Valiant m [r]
  FetchScalarF :: Statement p r -> p -> Valiant m r
  FetchOneOrThrowF :: Statement p r -> p -> Valiant m r
  FetchExistsF :: Statement p r -> p -> Valiant m Bool
  ExecuteF :: Statement p () -> p -> Valiant m Int64
  ExecuteReturningF :: Statement p r -> p -> Valiant m (Int64, [r])
  ExecuteBatchF :: Statement p () -> [p] -> Valiant m Int64
  WithTransactionF :: (Transaction -> IO a) -> Valiant m a
  WithConnectionF :: (Connection -> IO a) -> Valiant m a

------------------------------------------------------------------------
-- Carrier (via ReaderC Pool)
------------------------------------------------------------------------

-- | Pool-based carrier. Uses 'ReaderC Pool' internally.
type ValiantPoolC m = ReaderC Pool m

-- | Run the Valiant effect with a connection pool.
runValiantPool :: Pool -> ValiantPoolC m a -> m a
runValiantPool = runReader

------------------------------------------------------------------------
-- Smart constructors (interpret directly via reader + IO)
------------------------------------------------------------------------

-- | Fetch zero or one row.
fetchOneF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p r -> p -> m (Maybe r)
fetchOneF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Valiant.fetchOne conn stmt params

-- | Fetch all rows.
fetchAllF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p r -> p -> m [r]
fetchAllF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Valiant.fetchAll conn stmt params

-- | Fetch exactly one scalar value.
fetchScalarF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p r -> p -> m r
fetchScalarF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Valiant.fetchScalar conn stmt params

-- | Fetch one row, throwing if none returned.
fetchOneOrThrowF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p r -> p -> m r
fetchOneOrThrowF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Valiant.fetchOneOrThrow conn stmt params

-- | Check if a query returns any rows.
fetchExistsF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p r -> p -> m Bool
fetchExistsF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Valiant.fetchExists conn stmt params

-- | Execute a command. Returns rows affected.
executeF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p () -> p -> m Int64
executeF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Valiant.execute conn stmt params

-- | Execute with RETURNING.
executeReturningF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p r -> p -> m (Int64, [r])
executeReturningF stmt params = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Valiant.executeReturning conn stmt params

-- | Execute a batch of commands (pipelined).
executeBatchF :: (Has (Reader Pool) sig m, MonadIO m) => Statement p () -> [p] -> m Int64
executeBatchF stmt paramsList = do
  pool <- ask
  liftIO $ withResource pool $ \conn -> Valiant.executeBatch conn stmt paramsList

-- | Run an action in a transaction.
withTransactionF :: (Has (Reader Pool) sig m, MonadIO m) => (Transaction -> IO a) -> m a
withTransactionF action = do
  pool <- ask
  liftIO $ Valiant.withTransaction pool action

-- | Run an action with a raw connection.
withConnectionF :: (Has (Reader Pool) sig m, MonadIO m) => (Connection -> IO a) -> m a
withConnectionF action = do
  pool <- ask
  liftIO $ withResource pool action

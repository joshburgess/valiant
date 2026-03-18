-- | Optional convenience monad for hsqlx.
--
-- Provides a 'ReaderT'-based monad that carries a 'Pool' implicitly,
-- so you don't have to thread it through every function call.
-- Zero-cost at runtime ('ReaderT' is a newtype).
--
-- @
-- import Hsqlx.Monad
--
-- app :: Hsqlx ()
-- app = do
--   users <- fetchAllM Q.listUsers ()
--   mUser <- fetchOneM Q.findById 42
--   n     <- executeM Q.deleteOld cutoff
--   withTransactionM $ \\tx -> do
--     liftIO $ execute (txConn tx) Q.insert ("Alice", email)
--
-- main :: IO ()
-- main = do
--   pool <- newPool defaultPoolConfig { poolConnString = "..." }
--   runHsqlx pool app
-- @
--
-- All @*M@ functions acquire a connection from the pool, run the
-- operation, and return the connection automatically. For operations
-- that need multiple queries on the same connection, use
-- 'withConnectionM'.
module Hsqlx.Monad
  ( -- * Monad
    Hsqlx
  , runHsqlx

    -- * Connection access
  , askPool
  , withConnectionM
  , withResourceTimeoutM

    -- * Queries
  , fetchOneM
  , fetchAllM
  , fetchAllVecM
  , fetchScalarM
  , fetchOneOrThrowM
  , fetchOneOrM
  , fetchExistsM
  , forEachM

    -- * Commands
  , executeM
  , executeReturningM
  , executeReturningManyM
  , executeBatchM
  , executeManyM

    -- * Pipelined batch reads
  , fetchBatchOneM
  , fetchBatchAllM

    -- * Transactions
  , withTransactionM
  , withTransaction_M
  , withTransactionLevelM
  , withTransactionModeM
  , withReadOnlyTransactionM
  , withDeferrableTransactionM
  , withTransactionRetryM

    -- * Pool management
  , poolStatsM
  , resizeM
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Int (Int64)
import Data.Vector (Vector)
import Hsqlx.Execute (execute, executeBatch, executeMany, executeReturning, executeReturningMany, fetchAll, fetchAllVec, fetchBatchAll, fetchBatchOne, fetchExists, fetchOne, fetchOneOr, fetchOneOrThrow, fetchScalar, forEach)
import Hsqlx.Statement (Statement)
import Hsqlx.Transaction (IsolationLevel, Transaction, TransactionMode, withDeferrableTransaction, withReadOnlyTransaction, withTransaction, withTransactionLevel, withTransactionMode, withTransactionRetry, withTransaction_)
import PgWire.Connection (Connection)
import Data.Time (NominalDiffTime)
import PgWire.Pool (Pool, PoolStats, poolStats, resize, withResource, withResourceTimeout)

-- | A monad that carries a connection 'Pool' implicitly.
-- @Hsqlx a = ReaderT Pool IO a@.
type Hsqlx = ReaderT Pool IO

-- | Run an 'Hsqlx' action with the given pool.
runHsqlx :: Pool -> Hsqlx a -> IO a
runHsqlx pool action = runReaderT action pool
{-# INLINE runHsqlx #-}

-- | Get the underlying pool.
askPool :: Hsqlx Pool
askPool = ask
{-# INLINE askPool #-}

-- | Acquire a connection from the pool for the duration of the callback.
-- Useful when you need multiple operations on the same connection
-- without a transaction.
withConnectionM :: (Connection -> IO a) -> Hsqlx a
withConnectionM f = do
  pool <- ask
  liftIO $ withResource pool f
{-# INLINE withConnectionM #-}

-- | Like 'withConnectionM' but with a custom acquire timeout.
--
-- Overrides 'poolAcquireTimeout' for this single acquisition. Useful when
-- certain operations can tolerate longer (or shorter) waits than the pool
-- default.
withResourceTimeoutM :: NominalDiffTime -> (Connection -> IO a) -> Hsqlx a
withResourceTimeoutM timeout f = do
  pool <- ask
  liftIO $ withResourceTimeout pool timeout f
{-# INLINE withResourceTimeoutM #-}

------------------------------------------------------------------------
-- Queries
------------------------------------------------------------------------

-- | Fetch zero or one row.
fetchOneM :: Statement p r -> p -> Hsqlx (Maybe r)
fetchOneM stmt params = withConnectionM $ \conn -> fetchOne conn stmt params
{-# INLINE fetchOneM #-}

-- | Fetch all result rows.
fetchAllM :: Statement p r -> p -> Hsqlx [r]
fetchAllM stmt params = withConnectionM $ \conn -> fetchAll conn stmt params
{-# INLINE fetchAllM #-}

-- | Fetch a single scalar value.
fetchScalarM :: Statement p r -> p -> Hsqlx r
fetchScalarM stmt params = withConnectionM $ \conn -> fetchScalar conn stmt params
{-# INLINE fetchScalarM #-}

-- | Like 'fetchOneM' but throws 'DecodeError' if no rows are returned.
fetchOneOrThrowM :: Statement p r -> p -> Hsqlx r
fetchOneOrThrowM stmt params = withConnectionM $ \conn -> fetchOneOrThrow conn stmt params
{-# INLINE fetchOneOrThrowM #-}

-- | Fetch all result rows as a 'Vector'.
fetchAllVecM :: Statement p r -> p -> Hsqlx (Vector r)
fetchAllVecM stmt params = withConnectionM $ \conn -> fetchAllVec conn stmt params
{-# INLINE fetchAllVecM #-}

-- | Like 'fetchOneM' but returns a default value on no rows.
fetchOneOrM :: Statement p r -> p -> r -> Hsqlx r
fetchOneOrM stmt params def = withConnectionM $ \conn -> fetchOneOr conn stmt params def
{-# INLINE fetchOneOrM #-}

-- | Check whether a query returns any rows.
fetchExistsM :: Statement p r -> p -> Hsqlx Bool
fetchExistsM stmt params = withConnectionM $ \conn -> fetchExists conn stmt params
{-# INLINE fetchExistsM #-}

-- | Execute a callback for each result row.
forEachM :: Statement p r -> p -> (r -> IO ()) -> Hsqlx ()
forEachM stmt params action = withConnectionM $ \conn -> forEach conn stmt params action
{-# INLINE forEachM #-}

------------------------------------------------------------------------
-- Commands
------------------------------------------------------------------------

-- | Execute a command. Returns rows affected.
executeM :: Statement p () -> p -> Hsqlx Int64
executeM stmt params = withConnectionM $ \conn -> execute conn stmt params
{-# INLINE executeM #-}

-- | Execute a command with a RETURNING clause. Returns rows affected and decoded rows.
executeReturningM :: Statement p r -> p -> Hsqlx (Int64, [r])
executeReturningM stmt params = withConnectionM $ \conn -> executeReturning conn stmt params
{-# INLINE executeReturningM #-}

-- | Execute a batch of commands (pipelined). Returns total rows affected.
executeBatchM :: Statement p () -> [p] -> Hsqlx Int64
executeBatchM stmt paramsList = withConnectionM $ \conn -> executeBatch conn stmt paramsList
{-# INLINE executeBatchM #-}

-- | Execute a statement once for each parameter set. Returns total rows affected.
-- Alias for 'executeBatchM' with a more common name.
executeManyM :: Statement p () -> [p] -> Hsqlx Int64
executeManyM stmt paramsList = withConnectionM $ \conn -> executeMany conn stmt paramsList
{-# INLINE executeManyM #-}

-- | Execute a batch with RETURNING. Returns total rows and all decoded rows.
executeReturningManyM :: Statement p r -> [p] -> Hsqlx (Int64, [r])
executeReturningManyM stmt paramsList = withConnectionM $ \conn -> executeReturningMany conn stmt paramsList
{-# INLINE executeReturningManyM #-}

------------------------------------------------------------------------
-- Pipelined batch reads
------------------------------------------------------------------------

-- | Fetch zero or one row for each parameter set, pipelined.
fetchBatchOneM :: Statement p r -> [p] -> Hsqlx [Maybe r]
fetchBatchOneM stmt paramsList = withConnectionM $ \conn -> fetchBatchOne conn stmt paramsList
{-# INLINE fetchBatchOneM #-}

-- | Fetch all rows for each parameter set, pipelined.
fetchBatchAllM :: Statement p r -> [p] -> Hsqlx [[r]]
fetchBatchAllM stmt paramsList = withConnectionM $ \conn -> fetchBatchAll conn stmt paramsList
{-# INLINE fetchBatchAllM #-}

------------------------------------------------------------------------
-- Transactions
------------------------------------------------------------------------

-- | Run an action inside a transaction (READ COMMITTED).
withTransactionM :: (Transaction -> IO a) -> Hsqlx a
withTransactionM f = do
  pool <- ask
  liftIO $ withTransaction pool f
{-# INLINE withTransactionM #-}

-- | Like 'withTransactionM' but discards the return value.
withTransaction_M :: (Transaction -> IO ()) -> Hsqlx ()
withTransaction_M f = do
  pool <- ask
  liftIO $ withTransaction_ pool f
{-# INLINE withTransaction_M #-}

-- | Run an action inside a transaction with the given isolation level.
withTransactionLevelM :: IsolationLevel -> (Transaction -> IO a) -> Hsqlx a
withTransactionLevelM level f = do
  pool <- ask
  liftIO $ withTransactionLevel level pool f
{-# INLINE withTransactionLevelM #-}

-- | Run an action inside a transaction with a full 'TransactionMode'.
withTransactionModeM :: TransactionMode -> (Transaction -> IO a) -> Hsqlx a
withTransactionModeM mode f = do
  pool <- ask
  liftIO $ withTransactionMode mode pool f
{-# INLINE withTransactionModeM #-}

-- | Run an action inside a READ ONLY transaction.
withReadOnlyTransactionM :: (Transaction -> IO a) -> Hsqlx a
withReadOnlyTransactionM f = do
  pool <- ask
  liftIO $ withReadOnlyTransaction pool f
{-# INLINE withReadOnlyTransactionM #-}

-- | Run an action inside a SERIALIZABLE READ ONLY DEFERRABLE transaction.
withDeferrableTransactionM :: (Transaction -> IO a) -> Hsqlx a
withDeferrableTransactionM f = do
  pool <- ask
  liftIO $ withDeferrableTransaction pool f
{-# INLINE withDeferrableTransactionM #-}

-- | Run a SERIALIZABLE transaction with automatic retry on serialization failure.
withTransactionRetryM :: Int -> (Transaction -> IO a) -> Hsqlx a
withTransactionRetryM maxRetries f = do
  pool <- ask
  liftIO $ withTransactionRetry maxRetries pool f
{-# INLINE withTransactionRetryM #-}

------------------------------------------------------------------------
-- Pool management
------------------------------------------------------------------------

-- | Get a snapshot of the pool's statistics.
poolStatsM :: Hsqlx PoolStats
poolStatsM = do
  pool <- ask
  liftIO $ poolStats pool
{-# INLINE poolStatsM #-}

-- | Resize the pool at runtime.
resizeM :: Int -> Hsqlx ()
resizeM newSize = do
  pool <- ask
  liftIO $ resize pool newSize
{-# INLINE resizeM #-}

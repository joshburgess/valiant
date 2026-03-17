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

    -- * Queries
  , fetchOneM
  , fetchAllM
  , fetchScalarM

    -- * Commands
  , executeM
  , executeBatchM

    -- * Pipelined batch reads
  , fetchBatchOneM
  , fetchBatchAllM

    -- * Transactions
  , withTransactionM
  , withTransactionLevelM
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.Int (Int64)
import Hsqlx.Execute (execute, executeBatch, fetchAll, fetchBatchAll, fetchBatchOne, fetchOne, fetchScalar)
import Hsqlx.Statement (Statement)
import Hsqlx.Transaction (IsolationLevel, Transaction, withTransaction, withTransactionLevel)
import PgWire.Connection (Connection)
import PgWire.Pool (Pool, withResource)

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

------------------------------------------------------------------------
-- Commands
------------------------------------------------------------------------

-- | Execute a command. Returns rows affected.
executeM :: Statement p () -> p -> Hsqlx Int64
executeM stmt params = withConnectionM $ \conn -> execute conn stmt params
{-# INLINE executeM #-}

-- | Execute a batch of commands (pipelined). Returns total rows affected.
executeBatchM :: Statement p () -> [p] -> Hsqlx Int64
executeBatchM stmt paramsList = withConnectionM $ \conn -> executeBatch conn stmt paramsList
{-# INLINE executeBatchM #-}

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

-- | Run an action inside a transaction with the given isolation level.
withTransactionLevelM :: IsolationLevel -> (Transaction -> IO a) -> Hsqlx a
withTransactionLevelM level f = do
  pool <- ask
  liftIO $ withTransactionLevel level pool f
{-# INLINE withTransactionLevelM #-}

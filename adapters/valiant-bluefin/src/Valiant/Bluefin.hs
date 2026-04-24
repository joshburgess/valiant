-- | Bluefin effect adapter for valiant.
--
-- Bluefin uses explicit effect handles rather than type-level effect
-- lists. The 'ValiantHandle' provides database operations that can be
-- passed to functions explicitly.
--
-- @
-- import Bluefin.Eff
-- import Valiant.Bluefin
--
-- myApp :: ValiantHandle e -> Eff e [User]
-- myApp db = do
--   users <- fetchAllB db listUsers ()
--   pure users
--
-- main :: IO ()
-- main = do
--   pool <- newPool defaultPoolConfig { poolConnString = "..." }
--   result <- runValiantB pool $ \\db -> myApp db
--   print result
-- @
module Valiant.Bluefin
  ( -- * Handle
    ValiantHandle

    -- * Runner
  , runValiantB

    -- * Query operations
  , fetchOneB
  , fetchAllB
  , fetchScalarB
  , fetchOneOrThrowB
  , fetchExistsB

    -- * Command operations
  , executeB
  , executeReturningB
  , executeBatchB

    -- * Transaction operations
  , withTransactionB

    -- * Raw access
  , withConnectionB
  ) where

import Data.Int (Int64)
import Valiant (Connection, Pool, Statement, Transaction)
import Valiant qualified
import PgWire.Pool (withResource)

-- | An opaque handle to an valiant database connection pool.
-- Pass this explicitly to functions that need database access.
newtype ValiantHandle e = ValiantHandle { unHandle :: Pool }

-- | Run an action with an valiant database handle backed by a pool.
--
-- @
-- result <- runValiantB pool $ \\db -> do
--   users <- fetchAllB db listUsers ()
--   pure users
-- @
runValiantB :: Pool -> (forall e. ValiantHandle e -> IO a) -> IO a
runValiantB pool f = f (ValiantHandle pool)

-- | Fetch zero or one row.
fetchOneB :: ValiantHandle e -> Statement p r -> p -> IO (Maybe r)
fetchOneB h stmt params =
  withResource (unHandle h) $ \conn -> Valiant.fetchOne conn stmt params

-- | Fetch all rows.
fetchAllB :: ValiantHandle e -> Statement p r -> p -> IO [r]
fetchAllB h stmt params =
  withResource (unHandle h) $ \conn -> Valiant.fetchAll conn stmt params

-- | Fetch exactly one scalar value.
fetchScalarB :: ValiantHandle e -> Statement p r -> p -> IO r
fetchScalarB h stmt params =
  withResource (unHandle h) $ \conn -> Valiant.fetchScalar conn stmt params

-- | Fetch one row, throwing if none returned.
fetchOneOrThrowB :: ValiantHandle e -> Statement p r -> p -> IO r
fetchOneOrThrowB h stmt params =
  withResource (unHandle h) $ \conn -> Valiant.fetchOneOrThrow conn stmt params

-- | Check if a query returns any rows.
fetchExistsB :: ValiantHandle e -> Statement p r -> p -> IO Bool
fetchExistsB h stmt params =
  withResource (unHandle h) $ \conn -> Valiant.fetchExists conn stmt params

-- | Execute a command. Returns rows affected.
executeB :: ValiantHandle e -> Statement p () -> p -> IO Int64
executeB h stmt params =
  withResource (unHandle h) $ \conn -> Valiant.execute conn stmt params

-- | Execute with RETURNING.
executeReturningB :: ValiantHandle e -> Statement p r -> p -> IO (Int64, [r])
executeReturningB h stmt params =
  withResource (unHandle h) $ \conn -> Valiant.executeReturning conn stmt params

-- | Execute a batch of commands (pipelined).
executeBatchB :: ValiantHandle e -> Statement p () -> [p] -> IO Int64
executeBatchB h stmt paramsList =
  withResource (unHandle h) $ \conn -> Valiant.executeBatch conn stmt paramsList

-- | Run an action in a transaction.
withTransactionB :: ValiantHandle e -> (Transaction -> IO a) -> IO a
withTransactionB h = Valiant.withTransaction (unHandle h)

-- | Run an action with a raw connection.
withConnectionB :: ValiantHandle e -> (Connection -> IO a) -> IO a
withConnectionB h = withResource (unHandle h)

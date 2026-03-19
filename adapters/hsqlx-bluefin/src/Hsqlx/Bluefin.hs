-- | Bluefin effect adapter for hsqlx.
--
-- Bluefin uses explicit effect handles rather than type-level effect
-- lists. The 'HsqlxHandle' provides database operations that can be
-- passed to functions explicitly.
--
-- @
-- import Bluefin.Eff
-- import Hsqlx.Bluefin
--
-- myApp :: HsqlxHandle e -> Eff e [User]
-- myApp db = do
--   users <- fetchAllB db listUsers ()
--   pure users
--
-- main :: IO ()
-- main = do
--   pool <- newPool defaultPoolConfig { poolConnString = "..." }
--   result <- runHsqlxB pool $ \\db -> myApp db
--   print result
-- @
module Hsqlx.Bluefin
  ( -- * Handle
    HsqlxHandle

    -- * Runner
  , runHsqlxB

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
import Hsqlx (Connection, Pool, Statement, Transaction)
import Hsqlx qualified
import PgWire.Pool (withResource)

-- | An opaque handle to an hsqlx database connection pool.
-- Pass this explicitly to functions that need database access.
newtype HsqlxHandle e = HsqlxHandle { unHandle :: Pool }

-- | Run an action with an hsqlx database handle backed by a pool.
--
-- @
-- result <- runHsqlxB pool $ \\db -> do
--   users <- fetchAllB db listUsers ()
--   pure users
-- @
runHsqlxB :: Pool -> (forall e. HsqlxHandle e -> IO a) -> IO a
runHsqlxB pool f = f (HsqlxHandle pool)

-- | Fetch zero or one row.
fetchOneB :: HsqlxHandle e -> Statement p r -> p -> IO (Maybe r)
fetchOneB h stmt params =
  withResource (unHandle h) $ \conn -> Hsqlx.fetchOne conn stmt params

-- | Fetch all rows.
fetchAllB :: HsqlxHandle e -> Statement p r -> p -> IO [r]
fetchAllB h stmt params =
  withResource (unHandle h) $ \conn -> Hsqlx.fetchAll conn stmt params

-- | Fetch exactly one scalar value.
fetchScalarB :: HsqlxHandle e -> Statement p r -> p -> IO r
fetchScalarB h stmt params =
  withResource (unHandle h) $ \conn -> Hsqlx.fetchScalar conn stmt params

-- | Fetch one row, throwing if none returned.
fetchOneOrThrowB :: HsqlxHandle e -> Statement p r -> p -> IO r
fetchOneOrThrowB h stmt params =
  withResource (unHandle h) $ \conn -> Hsqlx.fetchOneOrThrow conn stmt params

-- | Check if a query returns any rows.
fetchExistsB :: HsqlxHandle e -> Statement p r -> p -> IO Bool
fetchExistsB h stmt params =
  withResource (unHandle h) $ \conn -> Hsqlx.fetchExists conn stmt params

-- | Execute a command. Returns rows affected.
executeB :: HsqlxHandle e -> Statement p () -> p -> IO Int64
executeB h stmt params =
  withResource (unHandle h) $ \conn -> Hsqlx.execute conn stmt params

-- | Execute with RETURNING.
executeReturningB :: HsqlxHandle e -> Statement p r -> p -> IO (Int64, [r])
executeReturningB h stmt params =
  withResource (unHandle h) $ \conn -> Hsqlx.executeReturning conn stmt params

-- | Execute a batch of commands (pipelined).
executeBatchB :: HsqlxHandle e -> Statement p () -> [p] -> IO Int64
executeBatchB h stmt paramsList =
  withResource (unHandle h) $ \conn -> Hsqlx.executeBatch conn stmt paramsList

-- | Run an action in a transaction.
withTransactionB :: HsqlxHandle e -> (Transaction -> IO a) -> IO a
withTransactionB h = Hsqlx.withTransaction (unHandle h)

-- | Run an action with a raw connection.
withConnectionB :: HsqlxHandle e -> (Connection -> IO a) -> IO a
withConnectionB h = withResource (unHandle h)

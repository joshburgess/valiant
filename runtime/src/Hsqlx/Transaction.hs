-- | Transaction management with savepoint support.
--
-- @
-- 'withTransaction' pool $ \\tx -> do
--   execute (txConn tx) insertUser (\"Alice\", email)
--   'withSavepoint' tx $ \\_ -> do
--     execute (txConn tx) riskyOperation params
--     -- If this throws, only the savepoint is rolled back,
--     -- not the entire transaction.
-- @
module Hsqlx.Transaction
  ( Transaction (..)
  , IsolationLevel (..)
  , withTransaction
  , withTransactionLevel
  , withSavepoint
  ) where

import Control.Exception (SomeException, catch, mask, onException)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Word (Word64)
import PgWire.Connection (Connection (..), simpleQuery)
import PgWire.Pool (Pool, withResource)
import System.IO.Unsafe (unsafePerformIO)

-- | A database transaction. Wraps a 'Connection'.
newtype Transaction = Transaction {txConn :: Connection}

-- | Transaction isolation level.
data IsolationLevel
  = ReadCommitted
  | RepeatableRead
  | Serializable
  deriving stock (Show, Eq)

-- | Run an action inside a transaction with default isolation (READ COMMITTED).
withTransaction :: Pool -> (Transaction -> IO a) -> IO a
withTransaction = withTransactionLevel ReadCommitted

-- | Run an action inside a transaction with the given isolation level.
withTransactionLevel :: IsolationLevel -> Pool -> (Transaction -> IO a) -> IO a
withTransactionLevel level pool action =
  withResource pool $ \conn -> mask $ \restore -> do
    _ <- simpleQuery conn (beginStatement level)
    result <- restore (action (Transaction conn)) `onException` rollback conn
    _ <- simpleQuery conn "COMMIT"
    pure result

-- | Run an action inside a savepoint within an existing transaction.
--
-- If the action throws an exception, the savepoint is rolled back but
-- the outer transaction remains active. If the action succeeds, the
-- savepoint is released.
--
-- Savepoints can be nested.
--
-- @
-- 'withTransaction' pool $ \\tx -> do
--   execute (txConn tx) stmt1 params1
--   result <- 'withSavepoint' tx $ \\_ -> do
--     execute (txConn tx) riskyStmt params2
--   -- If riskyStmt threw, we're still in the transaction
--   execute (txConn tx) stmt3 params3
-- @
withSavepoint :: Transaction -> (Transaction -> IO a) -> IO a
withSavepoint tx action = do
  name <- freshSavepointName
  let conn = txConn tx
  _ <- simpleQuery conn ("SAVEPOINT " <> name)
  mask $ \restore -> do
    result <- restore (action tx) `onException` do
      _ <- simpleQuery conn ("ROLLBACK TO SAVEPOINT " <> name)
      _ <- simpleQuery conn ("RELEASE SAVEPOINT " <> name)
      pure ()
    _ <- simpleQuery conn ("RELEASE SAVEPOINT " <> name)
    pure result

-- Internal ------------------------------------------------------------------

rollback :: Connection -> IO ()
rollback conn =
  (simpleQuery conn "ROLLBACK" >> pure ()) `catch` \(_ :: SomeException) -> pure ()

beginStatement :: IsolationLevel -> ByteString
beginStatement = \case
  ReadCommitted -> "BEGIN"
  RepeatableRead -> "BEGIN ISOLATION LEVEL REPEATABLE READ"
  Serializable -> "BEGIN ISOLATION LEVEL SERIALIZABLE"

-- | Global counter for unique savepoint names.
{-# NOINLINE savepointCounter #-}
savepointCounter :: IORef Word64
savepointCounter = unsafePerformIO (newIORef 0)

freshSavepointName :: IO ByteString
freshSavepointName = do
  n <- atomicModifyIORef' savepointCounter (\n -> (n + 1, n))
  pure ("hsqlx_sp_" <> BS8.pack (show n))

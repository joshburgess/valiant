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
module Valiant.Transaction
  ( Transaction (..)
  , IsolationLevel (..)
  , TransactionMode (..)
  , defaultTransactionMode
  , withTransaction
  , withTransaction_
  , withTransactionLevel
  , withTransactionMode
  , withTransactionConn
  , withTransactionLevelConn
  , withTransactionModeConn
  , withReadOnlyTransaction
  , withDeferrableTransaction
  , withTransactionRetry
  , withTransactionRetryIf
  , withSavepoint
  ) where

import Control.Exception (SomeException, catch, mask, onException, throwIO, try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Word (Word64)
import PgWire.Connection (Connection, simpleQuery)
import PgWire.Error (ValiantError (..))
import PgWire.Protocol.Backend (PgError (..))
import PgWire.Pool (Pool, withResource)
import System.IO.Unsafe (unsafePerformIO)

-- | A database transaction handle. Wraps a 'Connection' that has an active
-- @BEGIN@ block. Use 'txConn' to obtain the underlying connection for
-- executing queries within the transaction.
newtype Transaction = Transaction
  { txConn :: Connection
  -- ^ The underlying connection with an active transaction.
  }

-- | PostgreSQL transaction isolation level.
--
-- See the <https://www.postgresql.org/docs/current/transaction-iso.html PostgreSQL documentation>
-- for the semantics of each level.
data IsolationLevel
  = ReadCommitted
  -- ^ The default. Each statement sees a snapshot as of the start of that statement.
  | RepeatableRead
  -- ^ All statements in the transaction see a snapshot as of the first non-transaction-control
  -- statement. Throws a serialization error on write conflicts.
  | Serializable
  -- ^ The strictest level. Transactions behave as if executed sequentially.
  -- Throws a serialization error on detected conflicts.
  deriving stock (Show, Eq)

-- | Run an action inside a transaction with default isolation ('ReadCommitted').
--
-- Acquires a connection from the pool, issues @BEGIN@, runs the action, and
-- commits. If the action throws an exception, the transaction is rolled back
-- and the exception is re-raised. The connection is always returned to the pool.
--
-- @
-- withTransaction pool $ \\tx -> do
--   execute (txConn tx) insertStmt params
--   execute (txConn tx) updateStmt params
-- -- Auto-committed here, or rolled back on exception.
-- @
withTransaction :: Pool -> (Transaction -> IO a) -> IO a
withTransaction = withTransactionLevel ReadCommitted

-- | Like 'withTransaction' but discards the return value.
--
-- @
-- withTransaction_ pool $ \\tx ->
--   execute (txConn tx) insertStmt params
-- @
withTransaction_ :: Pool -> (Transaction -> IO ()) -> IO ()
withTransaction_ = withTransaction
{-# INLINE withTransaction_ #-}

-- | Run an action inside a transaction with the given isolation level.
--
-- Behaves like 'withTransaction' but issues @BEGIN ISOLATION LEVEL ...@ with
-- the specified level. Uses 'mask' to ensure the @BEGIN@\/@COMMIT@\/@ROLLBACK@
-- sequence cannot be interrupted by async exceptions.
--
-- If COMMIT fails (e.g., deferred constraint violation), the transaction is
-- rolled back so the connection is returned to the pool in a clean state.
-- Note: if the network drops after the server commits but before the client
-- receives the response, the client will attempt a ROLLBACK on an already-
-- committed transaction. This is an inherent TCP limitation — without two-
-- phase commit, the client cannot distinguish "committed but response lost"
-- from "failed to commit."
withTransactionLevel :: IsolationLevel -> Pool -> (Transaction -> IO a) -> IO a
withTransactionLevel level pool action =
  withResource pool $ \conn -> mask $ \restore -> do
    _ <- simpleQuery conn (beginStatement level)
    result <- restore (action (Transaction conn)) `onException` rollback conn
    _ <- simpleQuery conn "COMMIT" `onException` rollback conn
    pure result

-- | Run an action inside a transaction on an existing connection.
-- Unlike 'withTransaction' which acquires a connection from a pool, this
-- function operates on a connection you already hold (e.g., from 'withResource').
--
-- @
-- withResource pool $ \\conn -> do
--   -- some setup work on conn ...
--   withTransactionConn conn $ \\tx ->
--     execute (txConn tx) insertStmt params
-- @
withTransactionConn :: Connection -> (Transaction -> IO a) -> IO a
withTransactionConn = withTransactionLevelConn ReadCommitted

-- | Like 'withTransactionConn' but with a specific isolation level.
withTransactionLevelConn :: IsolationLevel -> Connection -> (Transaction -> IO a) -> IO a
withTransactionLevelConn level conn action = mask $ \restore -> do
  _ <- simpleQuery conn (beginStatement level)
  result <- restore (action (Transaction conn)) `onException` rollback conn
  _ <- simpleQuery conn "COMMIT" `onException` rollback conn
  pure result

-- | Full transaction mode configuration, bundling isolation level,
-- read/write mode, and deferrable flag.
--
-- @
-- let mode = defaultTransactionMode
--       { tmIsolation = Serializable
--       , tmReadOnly = True
--       , tmDeferrable = True
--       }
-- withTransactionMode mode pool $ \\tx -> ...
-- @
data TransactionMode = TransactionMode
  { tmIsolation :: !IsolationLevel
  -- ^ Isolation level. Default: 'ReadCommitted'.
  , tmReadOnly :: !Bool
  -- ^ If 'True', issues @READ ONLY@. Default: 'False'.
  , tmDeferrable :: !Bool
  -- ^ If 'True', issues @DEFERRABLE@. Only meaningful with
  -- 'Serializable' + 'tmReadOnly'. Default: 'False'.
  }
  deriving stock (Show, Eq)

-- | Default transaction mode: 'ReadCommitted', read-write, not deferrable.
defaultTransactionMode :: TransactionMode
defaultTransactionMode = TransactionMode
  { tmIsolation = ReadCommitted
  , tmReadOnly = False
  , tmDeferrable = False
  }

-- | Run an action inside a transaction with the given 'TransactionMode'.
--
-- This is the most general transaction function, supporting all combinations
-- of isolation level, read/write mode, and deferrable flag.
withTransactionMode :: TransactionMode -> Pool -> (Transaction -> IO a) -> IO a
withTransactionMode mode pool action =
  withResource pool $ \conn -> withTransactionModeConn mode conn action

-- | Like 'withTransactionMode' but on an existing connection.
withTransactionModeConn :: TransactionMode -> Connection -> (Transaction -> IO a) -> IO a
withTransactionModeConn mode conn action = mask $ \restore -> do
  _ <- simpleQuery conn (beginModeStatement mode)
  result <- restore (action (Transaction conn)) `onException` rollback conn
  _ <- simpleQuery conn "COMMIT" `onException` rollback conn
  pure result

-- | Run an action inside a @READ ONLY@ transaction. Postgres guarantees
-- no writes can occur, which enables use of standby replicas.
--
-- Uses the default 'ReadCommitted' isolation level. Combine with
-- 'withTransactionMode' if you need a different level.
--
-- @
-- users <- withReadOnlyTransaction pool $ \\tx ->
--   fetchAll (txConn tx) listAllUsers ()
-- @
withReadOnlyTransaction :: Pool -> (Transaction -> IO a) -> IO a
withReadOnlyTransaction =
  withTransactionMode defaultTransactionMode { tmReadOnly = True }

-- | Run an action inside a @SERIALIZABLE READ ONLY DEFERRABLE@ transaction.
--
-- PostgreSQL guarantees a consistent snapshot without risk of serialization
-- failure, making this ideal for long-running analytics or reporting queries.
-- May block briefly at the start while PostgreSQL finds a safe snapshot.
--
-- @
-- report <- withDeferrableTransaction pool $ \\tx ->
--   fetchAll (txConn tx) bigAnalyticsQuery ()
-- @
withDeferrableTransaction :: Pool -> (Transaction -> IO a) -> IO a
withDeferrableTransaction =
  withTransactionMode
    TransactionMode
      { tmIsolation = Serializable
      , tmReadOnly = True
      , tmDeferrable = True
      }

-- | Run a 'Serializable' transaction, automatically retrying on
-- serialization failures (SQLSTATE 40001) up to @maxRetries@ times.
--
-- The action is re-run from scratch on each retry (a new BEGIN is issued).
-- If all retries are exhausted, the last serialization error is thrown.
--
-- @
-- withTransactionRetry 3 pool $ \\tx -> do
--   balance <- fetchScalar (txConn tx) getBalance userId
--   execute (txConn tx) setBalance (balance + amount, userId)
-- @
withTransactionRetry :: Int -> Pool -> (Transaction -> IO a) -> IO a
withTransactionRetry maxRetries =
  withTransactionRetryIf isSerializationErr maxRetries
  where
    isSerializationErr (QueryError err) = pgCode err == "40001"
    isSerializationErr _ = False

-- | Like 'withTransactionRetry' but with a custom predicate to decide
-- which errors should trigger a retry.
--
-- @
-- -- Retry on both serialization failures and deadlocks
-- let shouldRetry err = isSerializationError err || isDeadlockError err
-- withTransactionRetryIf shouldRetry 3 pool $ \\tx -> ...
-- @
withTransactionRetryIf
  :: (ValiantError -> Bool)
  -> Int
  -> Pool
  -> (Transaction -> IO a)
  -> IO a
withTransactionRetryIf shouldRetry maxRetries pool action = go 0
  where
    go !attempt = do
      result <- try (withTransactionLevel Serializable pool action)
      case result of
        Right val -> pure val
        Left err
          | shouldRetry err && attempt < maxRetries -> go (attempt + 1)
          | otherwise -> throwIO err

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

beginModeStatement :: TransactionMode -> ByteString
beginModeStatement (TransactionMode iso ro def) =
  "BEGIN" <> isoClause <> roClause <> defClause
  where
    isoClause = case iso of
      ReadCommitted -> ""
      RepeatableRead -> " ISOLATION LEVEL REPEATABLE READ"
      Serializable -> " ISOLATION LEVEL SERIALIZABLE"
    roClause = if ro then " READ ONLY" else ""
    defClause = if def then " DEFERRABLE" else ""

-- | Global counter for unique savepoint names.
{-# NOINLINE savepointCounter #-}
savepointCounter :: IORef Word64
savepointCounter = unsafePerformIO (newIORef 0)

freshSavepointName :: IO ByteString
freshSavepointName = do
  n <- atomicModifyIORef' savepointCounter (\n -> (n + 1, n))
  pure ("valiant_sp_" <> BS8.pack (show n))

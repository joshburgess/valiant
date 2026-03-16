module Hsqlx.Transaction
  ( Transaction (..)
  , IsolationLevel (..)
  , withTransaction
  , withTransactionLevel
  ) where

import Control.Exception (SomeException, catch, mask, onException)
import Data.ByteString (ByteString)
import Hsqlx.Connection (Connection (..), simpleQuery)
import Hsqlx.Pool (Pool, withResource)

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

rollback :: Connection -> IO ()
rollback conn =
  (simpleQuery conn "ROLLBACK" >> pure ()) `catch` \(_ :: SomeException) -> pure ()

beginStatement :: IsolationLevel -> ByteString
beginStatement = \case
  ReadCommitted -> "BEGIN"
  RepeatableRead -> "BEGIN ISOLATION LEVEL REPEATABLE READ"
  Serializable -> "BEGIN ISOLATION LEVEL SERIALIZABLE"

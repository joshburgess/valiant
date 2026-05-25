-- | Structured error inspection and constraint violation helpers.
--
-- PostgreSQL errors carry a SQLSTATE code (5 ASCII characters) that
-- categorifies the error. This module provides helpers for extracting
-- the SQLSTATE, identifying constraint violations, and catching
-- specific error classes.
--
-- See <https://www.postgresql.org/docs/current/errcodes-appendix.html>
-- for the full list of SQLSTATE codes.
--
-- @
-- result <- try (execute conn insertStmt params)
-- case result of
--   Left err
--     | Just (UniqueViolation name) <- constraintViolation err ->
--         handleDuplicate name
--     | isSerializationError err ->
--         retry
--   Right n -> pure n
-- @
module Valiant.Error
  ( -- * SQLSTATE access
    sqlState
  , pgErrorOf

    -- * Constraint violations
  , ConstraintViolation (..)
  , constraintViolation
  , catchConstraintViolation

    -- * Error predicates
  , isUniqueViolation
  , isForeignKeyViolation
  , isNotNullViolation
  , isCheckViolation
  , isExclusionViolation
  , isSerializationError
  , isDeadlockError
  , isNoActiveTransactionError
  , isFailedTransactionError
  , isInvalidTextRepresentation
  , isUndefinedTable
  , isUndefinedColumn
  , isSyntaxError

    -- * Connection fatality
  , isFatal
  ) where

import Control.Exception (catch, throwIO)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import PgWire.Error (PgWireError (..), isFatal)
import PgWire.Protocol.Backend (PgError (..))

-- | Extract the SQLSTATE code from a 'PgWireError', if it wraps a
-- server-side 'QueryError'.
--
-- Returns 'Nothing' for non-query errors (connection errors, decode
-- errors, pool errors, etc.).
sqlState :: PgWireError -> Maybe ByteString
sqlState (QueryError err) = Just (pgCode err)
sqlState _ = Nothing

-- | Extract a field from the underlying 'PgError', if present.
--
-- @
-- mDetail <- pgErrorOf err pgDetail
-- mTable  <- pgErrorOf err pgTable
-- @
pgErrorOf :: PgWireError -> (PgError -> a) -> Maybe a
pgErrorOf (QueryError err) f = Just (f err)
pgErrorOf _ _ = Nothing

------------------------------------------------------------------------
-- Constraint violations
------------------------------------------------------------------------

-- | A structured constraint violation, extracted from the SQLSTATE code
-- and constraint name of a PostgreSQL error.
--
-- The 'ByteString' payload is the constraint name (from the @C@ error
-- field), which may be empty if PostgreSQL did not report it.
data ConstraintViolation
  = NotNullViolation !ByteString
  -- ^ @23502@ — a NOT NULL constraint was violated.
  | ForeignKeyViolation !ByteString
  -- ^ @23503@ — a foreign key constraint was violated.
  | UniqueViolation !ByteString
  -- ^ @23505@ — a unique constraint or unique index was violated.
  | CheckViolation !ByteString
  -- ^ @23514@ — a CHECK constraint was violated.
  | ExclusionViolation !ByteString
  -- ^ @23P01@ — an exclusion constraint was violated.
  deriving stock (Show, Eq, Ord)

-- | Extract a 'ConstraintViolation' from a 'PgWireError', if the error
-- is a constraint violation (SQLSTATE class 23).
--
-- Returns 'Nothing' for non-query errors or query errors that are not
-- constraint violations.
constraintViolation :: PgWireError -> Maybe ConstraintViolation
constraintViolation (QueryError err) =
  let code = pgCode err
      name = fromMaybe "" (pgConstraint err)
   in case code of
        "23502" -> Just (NotNullViolation name)
        "23503" -> Just (ForeignKeyViolation name)
        "23505" -> Just (UniqueViolation name)
        "23514" -> Just (CheckViolation name)
        "23P01" -> Just (ExclusionViolation name)
        _ -> Nothing
constraintViolation _ = Nothing

-- | Catch a constraint violation and handle it with a callback.
-- Non-constraint errors are re-thrown.
--
-- @
-- catchConstraintViolation
--   (\\err cv -> case cv of
--       UniqueViolation _ -> pure fallbackValue
--       _ -> throwIO err)
--   (execute conn insertStmt params)
-- @
catchConstraintViolation
  :: (PgWireError -> ConstraintViolation -> IO a)
  -> IO a
  -> IO a
catchConstraintViolation handler action =
  action `catch` \err ->
    case constraintViolation err of
      Just cv -> handler err cv
      Nothing -> throwIO err

------------------------------------------------------------------------
-- Error predicates
------------------------------------------------------------------------

-- | @23505@ — unique constraint or unique index violation.
isUniqueViolation :: PgWireError -> Bool
isUniqueViolation = hasState "23505"

-- | @23503@ — foreign key constraint violation.
isForeignKeyViolation :: PgWireError -> Bool
isForeignKeyViolation = hasState "23503"

-- | @23502@ — NOT NULL constraint violation.
isNotNullViolation :: PgWireError -> Bool
isNotNullViolation = hasState "23502"

-- | @23514@ — CHECK constraint violation.
isCheckViolation :: PgWireError -> Bool
isCheckViolation = hasState "23514"

-- | @23P01@ — exclusion constraint violation.
isExclusionViolation :: PgWireError -> Bool
isExclusionViolation = hasState "23P01"

-- | @40001@ — serialization failure. Retry the transaction.
isSerializationError :: PgWireError -> Bool
isSerializationError = hasState "40001"

-- | @40P01@ — deadlock detected. Retry the transaction.
isDeadlockError :: PgWireError -> Bool
isDeadlockError = hasState "40P01"

-- | @25P01@ — no active transaction (e.g., COMMIT outside a transaction).
isNoActiveTransactionError :: PgWireError -> Bool
isNoActiveTransactionError = hasState "25P01"

-- | @25P02@ — current transaction is aborted, commands ignored until
-- end of transaction block.
isFailedTransactionError :: PgWireError -> Bool
isFailedTransactionError = hasState "25P02"

-- | @22P02@ — invalid input syntax for type (e.g., passing @\"abc\"@ for an integer).
isInvalidTextRepresentation :: PgWireError -> Bool
isInvalidTextRepresentation = hasState "22P02"

-- | @42P01@ — undefined table.
isUndefinedTable :: PgWireError -> Bool
isUndefinedTable = hasState "42P01"

-- | @42703@ — undefined column.
isUndefinedColumn :: PgWireError -> Bool
isUndefinedColumn = hasState "42703"

-- | @42601@ — syntax error in SQL.
isSyntaxError :: PgWireError -> Bool
isSyntaxError = hasState "42601"

-- Internal: check if an error has a specific SQLSTATE code.
hasState :: ByteString -> PgWireError -> Bool
hasState expected err = sqlState err == Just expected

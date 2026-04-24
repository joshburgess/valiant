-- | Valiant — compile-time checked SQL for Haskell.
--
-- This is the main entry point for the runtime library. Import this module
-- to get access to all user-facing types and functions.
--
-- == Quick start
--
-- @
-- {\-\# OPTIONS_GHC -fplugin=Valiant.Plugin
--                 -fplugin-opt=Valiant.Plugin:sql-dir=sql \#-\}
--
-- module MyApp.Queries.Users where
--
-- import Valiant
--
-- -- sql\/users\/find_by_id.sql:
-- --   SELECT id, name, email FROM users WHERE id = $1
-- findById :: Statement Int32 (Maybe (Int32, Text, Maybe Text))
-- findById = queryFile \"users\/find_by_id.sql\"
-- @
--
-- == Runtime usage
--
-- @
-- pool <- 'newPool' 'defaultPoolConfig' { poolConnString = \"postgres:\/\/...\" }
--
-- -- Fetch one row
-- mUser <- 'withResource' pool $ \\conn ->
--   'fetchOne' conn findById 42
--
-- -- Batch insert (pipelined)
-- 'withResource' pool $ \\conn ->
--   'executeBatch' conn insertStmt [(\"Alice\", email1), (\"Bob\", email2)]
--
-- -- Transaction
-- 'withTransaction' pool $ \\tx ->
--   'execute' ('txConn' tx) insertStmt (\"Carol\", email3)
-- @
module Valiant
  ( -- * Statement
    -- | A 'Statement' represents a compile-time validated SQL query with
    -- typed parameters and results. Create them with 'queryFile' (validated
    -- by the GHC plugin) or 'mkStatement' (for manual construction).
    Statement (..)
  , query
  , queryFile
  , queryFileAs
  , mkStatement

    -- * Execution
    -- | Execute statements against a 'Connection'. All functions use the
    -- PostgreSQL extended query protocol with binary format encoding.
  , fetchOne
  , fetchAll
  , fetchAllVec
  , fetchAllWith
  , fetchScalar
  , fetchOneOrThrow
  , fetchOneOr
  , fetchFirst
  , fetchExists
  , forEach
    -- * Fast decode variants
    -- | Use these on hot paths with wide rows. 'FromRowFast' throws
    -- exceptions instead of returning 'Either', eliminating per-column
    -- allocation overhead. Derive via @Generic@ like 'FromRow'.
  , FromRowFast (..)
  , DecodeColumnFast (..)
  , fetchAllFast
  , fetchOneFast
  , execute
  , executeReturning
  , executeReturningMany
  , executeMany
  , executeBatch
  , fetchBatchOne
  , fetchBatchAll

    -- * Raw (unchecked) queries
    -- | Escape hatch for dynamic SQL or queries that don't fit the
    -- 'Statement' model. Uses the extended query protocol with
    -- binary-encoded parameters but no compile-time type checking.
    -- Parameters are pre-encoded 'ByteString' values with explicit OIDs.
  , rawFetchAll
  , rawFetchOne
  , rawExecute

    -- * Named parameters
    -- | Optional record-based parameter passing for queries using
    -- @:name@ syntax in SQL files. Derive 'ToNamedParams' via 'Generic'
    -- to pass a record whose field names match the SQL parameter names.
  , ToNamedParams (..)
  , NamedStatement
  , mkStatementNamed

    -- * Dynamic SQL
    -- | Build SQL at runtime for queries with dynamic WHERE clauses,
    -- optional filters, or variable structure.
    --
    -- Import "Valiant.Dynamic" directly for the full API (@sql@, @param@,
    -- @runSnippet@, etc.). The names are intentionally not re-exported
    -- here to avoid shadowing common local variable names.

    -- * UNNEST batch fetch
    -- | Fetch multiple entities by ID in a single query using
    -- @WHERE id = ANY($1::type[])@. Faster than pipelining for
    -- the common \"load N entities by ID\" pattern.
  , fetchByIds

    -- * Connection
    -- | Manage connections to PostgreSQL. Use 'connectString' for simple
    -- usage or 'connect' with a 'ConnConfig' for full control. For
    -- production use, prefer 'Pool' over direct connections.
  , Connection
  , ConnConfig (..)
  , TlsMode (..)
  , defaultConnConfig
  , connect
  , connectString
  , close
  , withConnection
  , simpleQuery

    -- * Pool
    -- | Thread-safe connection pool with configurable size, idle reaping,
    -- max lifetime, and health checking. Use 'withResource' to acquire
    -- and automatically release connections.
  , Pool
  , PoolConfig (..)
  , RecyclingMethod (..)
  , QueueMode (..)
  , defaultPoolConfig
  , newPool
  , closePool
  , drainPool
  , withResource
  , withResourceTimeout
  , newPoolFromString
  , PoolStats (..)
  , poolStats
  , poolIsAlive
  , resize
  , retain
  , setPostCreateHook
  , setOnAcquireHook
  , setPreReleaseHook
  , PoolLogger
  , nullPoolLogger
    -- | For structured pool event observations, import
    -- "PgWire.Pool.Observation" directly. Events include
    -- 'ConnectionCreated', 'ConnectionDestroyed', 'AcquireTimeout', etc.

    -- * Transactions
    -- | Run actions inside a database transaction. If an exception is
    -- thrown, the transaction is automatically rolled back.
  , Transaction (..)
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

    -- * Valiant monad (optional convenience)
    -- | For the @ReaderT Pool IO@ monad with lifted @*M@ functions,
    -- install @valiant-mtl@ and import "Valiant.Monad".
    -- For generic MTL-style operations, import "Valiant.Mtl".

    -- * Large objects
    -- | Streaming interface for binary data too large for a single column.
    -- All operations must occur within a transaction.
  , LoFd (..)
  , LoMode (..)
  , loCreate
  , loUnlink
  , loOpen
  , loClose
  , withLargeObject
  , loRead
  , loWrite
  , loSeek
  , loTell
  , loTruncate
  , loImport
  , loExport

    -- * Advisory locks
    -- | PostgreSQL advisory locks for distributed coordination.
    -- Session-scoped locks require explicit release; transaction-scoped
    -- locks are released automatically on COMMIT\/ROLLBACK.
  , withAdvisoryLock
  , withAdvisoryLockTry
  , advisoryLock
  , advisoryUnlock
  , withAdvisoryLockTx
  , withAdvisoryLockTxTry
  , advisoryLockTx

    -- * Cancellation
    -- | Cancel in-flight queries. 'cancelQuery' opens a separate TCP
    -- connection and sends a CancelRequest to the server.
    -- 'withQueryTimeout' wraps any action with a deadline.
  , cancelQuery
  , withQueryTimeout

    -- * Streaming fold (constant memory)
    -- | Process large result sets without buffering, no cursor needed.
  , RowFold (..)
  , executeWithFold

    -- * Pipelined reads
    -- | Execute multiple independent queries in a single round-trip
    -- using the 'Applicative' interface. Eliminates N+1 query overhead.
  , Pipeline
  , pipeFetchOne
  , pipeFetchAll
  , pipeFetchScalar
  , pipeExecute
  , runPipeline

    -- * Streaming (cursors)
    -- | Stream large result sets using server-side cursors, fetching
    -- rows in batches without loading everything into memory.
    -- Must be used inside a transaction.
  , CursorState (..)
  , withCursor
  , fetchBatch

    -- * LISTEN\/NOTIFY
    -- | Subscribe to PostgreSQL asynchronous notification channels.
  , Notification (..)
  , listen
  , unlisten
  , waitForNotification
  , waitForNotificationTimeout

    -- * COPY
    -- | Bulk data import\/export using the PostgreSQL COPY protocol.
  , CopyResult (..)
  , copyIn
  , copyInBinary
  , copyOut

    -- * Logging
    -- | Hooks for instrumenting query timing and connection events.
  , LogEvent (..)
  , LogLevel (..)
  , Logger
  , nullLogger
  , stderrLogger
  , poolLoggerFromLogger

    -- * Row decoding
    -- | Decode result rows into Haskell types. Instances are provided
    -- for tuples up to 6 elements, 'Maybe' for nullable columns, and
    -- @()@ for commands that return no rows.
  , FromRow (..)
  , FromRowStrict (..)
  , DecodeColumn (..)

    -- * Parameter encoding
    -- | Encode query parameters. Instances are provided for single
    -- values, 'Maybe' for nullable parameters, @()@ for no parameters,
    -- and tuples up to 6 elements.
  , ToParams (..)
  , EncodeField (..)

    -- * Error inspection
    -- | Structured error inspection, SQLSTATE access, and constraint
    -- violation helpers. See "Valiant.Error" for the full API.
  , ConstraintViolation (..)
  , constraintViolation
  , catchConstraintViolation
  , sqlState
  , pgErrorOf
  , isUniqueViolation
  , isForeignKeyViolation
  , isSerializationError
  , isDeadlockError

    -- * Binary codec type classes
    -- | Low-level encode\/decode classes for individual PostgreSQL types.
    -- Derive these for newtypes using @GeneralizedNewtypeDeriving@ or
    -- @DerivingVia@:
    --
    -- @
    -- newtype UserId = UserId Int32
    --   deriving newtype (PgEncode, PgDecode)
    -- @
  , PgEncode (..)
  , PgDecode (..)

    -- * Binary types
    -- | PostgreSQL types that don't have a standard Haskell equivalent.
  , PgInet (..)
  , ipv4
  , ipv4Host
  , ipv6
  , ipv6Host
  , inetToText
  , PgMacAddr (..)
  , macAddr
  , macAddrToText
  , PgHStore (..)
  , hstoreFromList
  , hstoreToList
  , Unbounded (..)
  , finite
  , fromFinite
  , PgPoint (..)
  , PgInterval (..)
  , PgRange (..)
  , RangeBound (..)
  , CompositeField (..)

    -- * Re-exports
    -- | 'Generic' is re-exported for convenience so that users can derive
    -- 'FromRow' and 'ToNamedParams' without an extra import.
  , Generic
  ) where

import GHC.Generics (Generic)
import Valiant.Advisory (advisoryLock, advisoryLockTx, advisoryUnlock, withAdvisoryLock, withAdvisoryLockTry, withAdvisoryLockTx, withAdvisoryLockTxTry)
import Valiant.Batch (fetchByIds)
import Valiant.Binary.Composite (CompositeField (..))
import Valiant.Binary.HStore (PgHStore (..), hstoreFromList, hstoreToList)
import Valiant.Binary.Inet (PgInet (..), ipv4, ipv4Host, ipv6, ipv6Host, inetToText)
import Valiant.Binary.MacAddr (PgMacAddr (..), macAddr, macAddrToText)
import Valiant.Binary.Point (PgPoint (..))
import Valiant.Binary.Unbounded (Unbounded (..), finite, fromFinite)
import Valiant.Binary.Interval (PgInterval (..))
import Valiant.Binary.Range (PgRange (..), RangeBound (..))
import Valiant.Copy (CopyResult (..), copyIn, copyInBinary, copyOut)
import Valiant.Dynamic ()
import Valiant.Error (ConstraintViolation (..), catchConstraintViolation, constraintViolation, isDeadlockError, isForeignKeyViolation, isSerializationError, isUniqueViolation, pgErrorOf, sqlState)
import Valiant.Execute (execute, executeBatch, executeMany, executeReturning, executeReturningMany, fetchAll, fetchAllFast, fetchAllVec, fetchAllWith, fetchBatchAll, fetchBatchOne, fetchExists, fetchFirst, fetchOne, fetchOneFast, fetchOneOr, fetchOneOrThrow, fetchScalar, forEach, rawExecute, rawFetchAll, rawFetchOne)
import Valiant.FromRowFast (FromRowFast (..), DecodeColumnFast (..))
import Valiant.Fold (RowFold (..), executeWithFold)
import Valiant.LargeObject (LoFd (..), LoMode (..), loClose, loCreate, loExport, loImport, loOpen, loRead, loSeek, loTell, loTruncate, loUnlink, loWrite, withLargeObject)
import Valiant.FromRow (DecodeColumn (..), FromRow (..))
import Valiant.FromRowStrict (FromRowStrict (..))
import Valiant.Logging (LogEvent (..), LogLevel (..), Logger, nullLogger, poolLoggerFromLogger, stderrLogger)
import Valiant.NamedParams (NamedStatement, ToNamedParams (..), mkStatementNamed)
import Valiant.Notify (Notification (..), listen, unlisten, waitForNotification, waitForNotificationTimeout)
import Valiant.Pipeline (Pipeline, pipeFetchOne, pipeFetchAll, pipeFetchScalar, pipeExecute, runPipeline)
import Valiant.Statement (Statement (..), mkStatement, query, queryFile, queryFileAs)
import Valiant.Streaming (CursorState (..), fetchBatch, withCursor)
import Valiant.ToParams (EncodeField (..), ToParams (..))
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import Valiant.Transaction (IsolationLevel (..), Transaction (..), TransactionMode (..), defaultTransactionMode, withDeferrableTransaction, withReadOnlyTransaction, withSavepoint, withTransaction, withTransactionConn, withTransactionLevel, withTransactionLevelConn, withTransactionMode, withTransactionModeConn, withTransactionRetry, withTransactionRetryIf, withTransaction_)
import PgWire.Cancel (cancelQuery, withQueryTimeout)
import PgWire.Connection (Connection, close, connect, connectString, simpleQuery, withConnection)
import PgWire.Connection.Config (ConnConfig (..), TlsMode (..), defaultConnConfig)
import PgWire.Pool (Pool, PoolStats (..), closePool, drainPool, newPool, poolIsAlive, poolStats, resize, retain, setPostCreateHook, setOnAcquireHook, setPreReleaseHook, withResource, withResourceTimeout)
import PgWire.Pool.Config (PoolConfig (..), PoolLogger, QueueMode (..), RecyclingMethod (..), defaultPoolConfig, nullPoolLogger)
import PgWire.Pool.Observation ()

import Data.ByteString (ByteString)

-- | Create a connection pool from a connection string with default settings.
--
-- Equivalent to @'newPool' 'defaultPoolConfig' { poolConnString = connStr }@.
--
-- @
-- pool <- newPoolFromString \"postgres:\/\/user:pass\@localhost:5432\/mydb\"
-- @
newPoolFromString :: ByteString -> IO Pool
newPoolFromString connStr = newPool defaultPoolConfig { poolConnString = connStr }

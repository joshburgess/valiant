-- | Hsqlx — compile-time checked SQL for Haskell.
--
-- This is the main entry point for the runtime library. Import this module
-- to get access to all user-facing types and functions.
--
-- == Quick start
--
-- @
-- {\-\# OPTIONS_GHC -fplugin=Hsqlx.Plugin
--                 -fplugin-opt=Hsqlx.Plugin:sql-dir=sql \#-\}
--
-- module MyApp.Queries.Users where
--
-- import Hsqlx
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
module Hsqlx
  ( -- * Statement
    -- | A 'Statement' represents a compile-time validated SQL query with
    -- typed parameters and results. Create them with 'queryFile' (validated
    -- by the GHC plugin) or 'mkStatement' (for manual construction).
    Statement (..)
  , queryFile
  , queryFileAs
  , mkStatement

    -- * Execution
    -- | Execute statements against a 'Connection'. All functions use the
    -- PostgreSQL extended query protocol with binary format encoding.
  , fetchOne
  , fetchAll
  , fetchScalar
  , execute
  , executeBatch
  , fetchBatchOne
  , fetchBatchAll

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
  , defaultPoolConfig
  , newPool
  , closePool
  , withResource

    -- * Transactions
    -- | Run actions inside a database transaction. If an exception is
    -- thrown, the transaction is automatically rolled back.
  , Transaction (..)
  , IsolationLevel (..)
  , withTransaction
  , withTransactionLevel
  , withSavepoint

    -- * Cancellation
    -- | Cancel in-flight queries. 'cancelQuery' opens a separate TCP
    -- connection and sends a CancelRequest to the server.
    -- 'withQueryTimeout' wraps any action with a deadline.
  , cancelQuery
  , withQueryTimeout

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
  , copyOut

    -- * Logging
    -- | Hooks for instrumenting query timing and connection events.
  , LogEvent (..)
  , LogLevel (..)
  , Logger
  , nullLogger
  , stderrLogger

    -- * Row decoding
    -- | Decode result rows into Haskell types. Instances are provided
    -- for tuples up to 6 elements, 'Maybe' for nullable columns, and
    -- @()@ for commands that return no rows.
  , FromRow (..)

    -- * Parameter encoding
    -- | Encode query parameters. Instances are provided for single
    -- values, 'Maybe' for nullable parameters, @()@ for no parameters,
    -- and tuples up to 6 elements.
  , ToParams (..)
  , EncodeField (..)

    -- * Binary types
    -- | PostgreSQL types that don't have a standard Haskell equivalent.
  , PgInterval (..)
  , PgRange (..)
  , RangeBound (..)
  , CompositeField (..)
  ) where

import Hsqlx.Batch (fetchByIds)
import Hsqlx.Binary.Composite (CompositeField (..))
import Hsqlx.Binary.Interval (PgInterval (..))
import Hsqlx.Binary.Range (PgRange (..), RangeBound (..))
import Hsqlx.Copy (CopyResult (..), copyIn, copyOut)
import Hsqlx.Execute (execute, executeBatch, fetchAll, fetchBatchAll, fetchBatchOne, fetchOne, fetchScalar)
import Hsqlx.FromRow (FromRow (..))
import Hsqlx.Logging (LogEvent (..), LogLevel (..), Logger, nullLogger, stderrLogger)
import Hsqlx.Notify (Notification (..), listen, unlisten, waitForNotification, waitForNotificationTimeout)
import Hsqlx.Pipeline (Pipeline, pipeFetchOne, pipeFetchAll, pipeFetchScalar, pipeExecute, runPipeline)
import Hsqlx.Statement (Statement (..), mkStatement, queryFile, queryFileAs)
import Hsqlx.Streaming (CursorState (..), fetchBatch, withCursor)
import Hsqlx.ToParams (EncodeField (..), ToParams (..))
import Hsqlx.Transaction (IsolationLevel (..), Transaction (..), withSavepoint, withTransaction, withTransactionLevel)
import PgWire.Cancel (cancelQuery, withQueryTimeout)
import PgWire.Connection (Connection, close, connect, connectString, simpleQuery, withConnection)
import PgWire.Connection.Config (ConnConfig (..), TlsMode (..), defaultConnConfig)
import PgWire.Pool (Pool, closePool, newPool, withResource)
import PgWire.Pool.Config (PoolConfig (..), defaultPoolConfig)

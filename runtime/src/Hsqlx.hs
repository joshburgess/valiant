-- | Hsqlx — compile-time checked SQL for Haskell.
--
-- This is the main entry point for the runtime library.
-- Import this module to get access to all user-facing types and functions.
module Hsqlx
  ( -- * Statement
    Statement (..)
  , queryFile
  , queryFileAs
  , mkStatement

    -- * Execution
  , fetchOne
  , fetchAll
  , fetchScalar
  , execute
  , executeBatch

    -- * Connection
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
  , Pool
  , PoolConfig (..)
  , defaultPoolConfig
  , newPool
  , closePool
  , withResource

    -- * Transactions
  , Transaction (..)
  , IsolationLevel (..)
  , withTransaction
  , withTransactionLevel

    -- * Streaming (cursors)
  , CursorState (..)
  , withCursor
  , fetchBatch

    -- * LISTEN/NOTIFY
  , Notification (..)
  , listen
  , unlisten
  , waitForNotification
  , waitForNotificationTimeout

    -- * COPY
  , CopyResult (..)
  , copyIn
  , copyOut

    -- * Logging
  , LogEvent (..)
  , LogLevel (..)
  , Logger
  , nullLogger
  , stderrLogger

    -- * Row decoding
  , FromRow (..)

    -- * Parameter encoding
  , ToParams (..)
  , EncodeField (..)

    -- * Binary types
  , PgInterval (..)
  , PgRange (..)
  , RangeBound (..)
  , CompositeField (..)
  ) where

import Hsqlx.Binary.Composite (CompositeField (..))
import Hsqlx.Binary.Interval (PgInterval (..))
import Hsqlx.Binary.Range (PgRange (..), RangeBound (..))
import Hsqlx.Connection (Connection, close, connect, connectString, simpleQuery, withConnection)
import Hsqlx.Connection.Config (ConnConfig (..), TlsMode (..), defaultConnConfig)
import Hsqlx.Copy (CopyResult (..), copyIn, copyOut)
import Hsqlx.Execute (execute, executeBatch, fetchAll, fetchOne, fetchScalar)
import Hsqlx.FromRow (FromRow (..))
import Hsqlx.Logging (LogEvent (..), LogLevel (..), Logger, nullLogger, stderrLogger)
import Hsqlx.Notify (Notification (..), listen, unlisten, waitForNotification, waitForNotificationTimeout)
import Hsqlx.Pool (Pool, closePool, newPool, withResource)
import Hsqlx.Pool.Config (PoolConfig (..), defaultPoolConfig)
import Hsqlx.Statement (Statement (..), mkStatement, queryFile, queryFileAs)
import Hsqlx.Streaming (CursorState (..), fetchBatch, withCursor)
import Hsqlx.ToParams (EncodeField (..), ToParams (..))
import Hsqlx.Transaction (IsolationLevel (..), Transaction (..), withTransaction, withTransactionLevel)

-- | Error types for hsqlx runtime exceptions.
--
-- All errors are thrown as 'HsqlxError' via 'throwIO' and can be caught
-- with the standard @Control.Exception@ machinery.
module PgWire.Error
  ( HsqlxError (..)
  , throwHsqlx
  ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import PgWire.Protocol.Backend (PgError)

-- | All runtime errors thrown by hsqlx.
data HsqlxError
  = ConnectionError ByteString
  -- ^ Failed to establish a TCP\/TLS connection to the server.
  | AuthError ByteString
  -- ^ Authentication failed (wrong password, unsupported mechanism, etc.).
  | ProtocolError ByteString
  -- ^ Unexpected message in the wire protocol (indicates a driver bug or
  -- incompatible server).
  | QueryError PgError
  -- ^ The server returned an error in response to a query. The 'PgError'
  -- contains SQLSTATE, message, detail, hint, and position fields.
  | DecodeError ByteString
  -- ^ Failed to decode a result row (type mismatch, unexpected NULL, or
  -- no rows returned to 'fetchOneOrThrow').
  | PoolTimeout
  -- ^ Timed out waiting to acquire a connection from the pool.
  | PoolClosed
  -- ^ Attempted to use a pool after 'PgWire.Pool.closePool' was called.
  | ConnectionDead
  -- ^ The connection's reader thread has died (network failure, server crash).
  deriving stock (Show, Eq)

instance Exception HsqlxError

-- | Throw an 'HsqlxError' as an exception.
throwHsqlx :: HsqlxError -> IO a
throwHsqlx = throwIO

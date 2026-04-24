-- | Error types for pg-wire runtime exceptions.
--
-- All errors are thrown as 'PgWireError' via 'throwIO' and can be caught
-- with the standard @Control.Exception@ machinery.
module PgWire.Error
  ( PgWireError (..)
  , throwPgWire
  ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import PgWire.Protocol.Backend (PgError)

-- | All runtime errors thrown by pg-wire.
data PgWireError
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
  deriving stock (Show, Eq, Generic)

instance NoThunks PgWireError
instance Exception PgWireError

-- | Throw a 'PgWireError' as an exception.
throwPgWire :: PgWireError -> IO a
throwPgWire = throwIO

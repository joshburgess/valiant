-- | Error types for pg-wire runtime exceptions.
--
-- All errors are thrown as 'PgWireError' via 'throwIO' and can be caught
-- with the standard @Control.Exception@ machinery.
module PgWire.Error
  ( PgWireError (..)
  , throwPgWire
  , isFatal
  ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import PgWire.Protocol.Backend (PgError (..))

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

-- | Classify whether an error indicates the connection is unusable.
--
-- Fatal errors mean the connection should be destroyed rather than returned
-- to the pool. Recoverable errors (constraint violations, syntax errors,
-- serialization failures) leave the connection in a usable state.
--
-- Fatal error classes:
--
-- * 'ConnectionError', 'AuthError', 'ProtocolError', 'ConnectionDead' —
--   always fatal.
-- * SQLSTATE class @08@ — connection exception (server closed it).
-- * SQLSTATE class @57@ — operator intervention (admin shutdown, crash recovery).
-- * SQLSTATE class @58@ — system error (I\/O error, out of memory on server).
-- * SQLSTATE @F0000@ — configuration file error.
-- * SQLSTATE @XX000@ — internal error (server bug).
--
-- Everything else (constraint violations, query errors, transaction errors,
-- syntax errors, etc.) is recoverable.
isFatal :: PgWireError -> Bool
isFatal ConnectionError{} = True
isFatal AuthError{} = True
isFatal ProtocolError{} = True
isFatal ConnectionDead{} = True
isFatal PoolTimeout{} = False
isFatal PoolClosed{} = False
isFatal DecodeError{} = False
isFatal (QueryError err) = isFatalState (pgCode err)

-- | Check if a SQLSTATE code indicates a fatal error.
isFatalState :: ByteString -> Bool
isFatalState code
  | BS.length code >= 2 =
      let cls = BS.take 2 code
       in cls == "08"     -- connection exception
       || cls == "57"     -- operator intervention
       || cls == "58"     -- system error
       || code == "F0000" -- configuration file error
       || code == "XX000" -- internal error
  | otherwise = False

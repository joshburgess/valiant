module Hsqlx.Error
  ( HsqlxError (..)
  , throwHsqlx
  ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Hsqlx.Protocol.Backend (PgError)

-- | All runtime errors thrown by hsqlx.
data HsqlxError
  = ConnectionError ByteString
  | AuthError ByteString
  | ProtocolError ByteString
  | QueryError PgError
  | DecodeError ByteString
  | PoolTimeout
  | PoolClosed
  deriving stock (Show, Eq)

instance Exception HsqlxError

-- | Throw an 'HsqlxError' as an exception.
throwHsqlx :: HsqlxError -> IO a
throwHsqlx = throwIO

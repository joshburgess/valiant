module PgWire.Auth.Cleartext
  ( cleartextAuth
  ) where

import Data.ByteString (ByteString)
import PgWire.Protocol.Frontend (FrontendMsg (..))
import PgWire.Wire (WireConn, sendFrontendMsg)

-- | Handle cleartext password authentication.
cleartextAuth :: WireConn -> ByteString -> IO ()
cleartextAuth wc password =
  sendFrontendMsg wc (PasswordMessage password)

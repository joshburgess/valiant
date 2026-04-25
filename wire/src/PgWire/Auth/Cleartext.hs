-- | Cleartext password authentication.
--
-- __Audience:__ exposed for downstream library authors. The standard
-- authentication flow is driven from "PgWire.Connection"; this module
-- is only useful if you are reimplementing handshake.
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

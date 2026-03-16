module Hsqlx.Auth.Cleartext
  ( cleartextAuth
  ) where

import Data.ByteString (ByteString)
import Hsqlx.Protocol.Frontend (FrontendMsg (..))
import Hsqlx.Wire (WireConn, sendFrontendMsg)

-- | Handle cleartext password authentication.
cleartextAuth :: WireConn -> ByteString -> IO ()
cleartextAuth wc password =
  sendFrontendMsg wc (PasswordMessage password)

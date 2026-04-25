-- | MD5 password authentication (legacy PostgreSQL auth method).
--
-- __Audience:__ exposed for downstream library authors. The standard
-- authentication flow is driven from "PgWire.Connection"; this module
-- is only useful if you are reimplementing handshake.
module PgWire.Auth.MD5
  ( md5Auth
  , md5Password
  ) where

import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import PgWire.Protocol.Frontend (FrontendMsg (..))
import PgWire.Wire (WireConn, sendFrontendMsg)
import Numeric (showHex)

-- | Handle MD5 password authentication.
-- PG expects: @"md5" ++ md5(md5(password ++ user) ++ salt)@
md5Auth :: WireConn -> ByteString -> ByteString -> ByteString -> IO ()
md5Auth wc user password salt =
  sendFrontendMsg wc (PasswordMessage (md5Password user password salt))

-- | Compute the MD5 password hash.
md5Password :: ByteString -> ByteString -> ByteString -> ByteString
md5Password user password salt =
  let inner = hexMd5 (password <> user)
      outer = hexMd5 (inner <> salt)
   in "md5" <> outer

hexMd5 :: ByteString -> ByteString
hexMd5 = BS.pack . concatMap toHexBytes . BS.unpack . MD5.hash

toHexBytes :: Word8 -> [Word8]
toHexBytes w =
  let s = showHex w ""
      padded = if length s == 1 then '0' : s else s
   in map (fromIntegral . fromEnum) padded

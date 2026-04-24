module Valiant.Plugin.Hash
  ( sha256Hex
  ) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Numeric (showHex)

-- | Full SHA-256 hex digest of a 'ByteString'.
sha256Hex :: ByteString -> Text
sha256Hex = T.pack . concatMap toHex . BS.unpack . SHA256.hash

toHex :: Word8 -> String
toHex w
  | w < 16 = '0' : showHex w ""
  | otherwise = showHex w ""

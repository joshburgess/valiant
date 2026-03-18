{-# OPTIONS_GHC -Wno-orphans #-}

-- | Binary codec for PostgreSQL @macaddr@ and @macaddr8@ types.
--
-- PG binary format:
--   @macaddr@: 6 raw bytes (OID 829)
--   @macaddr8@: 8 raw bytes (OID 774)
module Hsqlx.Binary.MacAddr
  ( PgMacAddr (..)
  , macAddr
  , macAddrToText
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric (showHex)
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import PgWire.Protocol.Oid (oidMacAddr)

-- | A PostgreSQL @macaddr@ value (6 bytes) or @macaddr8@ value (8 bytes).
data PgMacAddr = PgMacAddr
  { macAddrBytes :: !ByteString
  -- ^ Raw MAC address bytes: 6 bytes for @macaddr@, 8 bytes for @macaddr8@.
  }
  deriving stock (Eq, Ord, Generic)

instance NFData PgMacAddr

instance Show PgMacAddr where
  show = T.unpack . macAddrToText

-- | Construct a @macaddr@ from 6 bytes.
macAddr :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> PgMacAddr
macAddr a b c d e f = PgMacAddr (BS.pack [a, b, c, d, e, f])

-- | Render a 'PgMacAddr' as colon-separated hex text.
--
-- >>> macAddrToText (macAddr 0x08 0x00 0x2b 0x01 0x02 0x03)
-- "08:00:2b:01:02:03"
macAddrToText :: PgMacAddr -> Text
macAddrToText (PgMacAddr bs) =
  T.intercalate ":" $
    map (\i -> let w = BS.index bs i in T.pack (pad2 (showHex w "")))
        [0 .. BS.length bs - 1]
  where
    pad2 [c] = ['0', c]
    pad2 s = s

instance PgEncode PgMacAddr where
  pgEncode (PgMacAddr bs) = bs
  {-# INLINE pgEncode #-}
  pgOid _ = oidMacAddr
  {-# INLINE pgOid #-}

instance PgDecode PgMacAddr where
  pgDecode bs
    | len == 6 || len == 8 = Right (PgMacAddr bs)
    | otherwise = Left $ "macaddr: expected 6 or 8 bytes, got " <> show len
    where len = BS.length bs
  {-# INLINE pgDecode #-}

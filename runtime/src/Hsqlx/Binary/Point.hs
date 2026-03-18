{-# OPTIONS_GHC -Wno-orphans #-}

-- | Binary codec for PostgreSQL @point@ type (OID 600).
--
-- PG binary format: two float8 values (x, y), 16 bytes total.
module Hsqlx.Binary.Point
  ( PgPoint (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word64)
import GHC.Float (castWord64ToDouble, castDoubleToWord64)
import GHC.Generics (Generic)
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import PgWire.Protocol.Oid (Oid (..))

-- | A PostgreSQL @point@ value — an (x, y) coordinate pair.
data PgPoint = PgPoint
  { pointX :: {-# UNPACK #-} !Double
  , pointY :: {-# UNPACK #-} !Double
  }
  deriving stock (Show, Eq, Ord, Generic)

instance NFData PgPoint

oidPoint :: Oid
oidPoint = Oid 600

instance PgEncode PgPoint where
  pgEncode (PgPoint x y) =
    LBS.toStrict . B.toLazyByteString $
      B.word64BE (castDoubleToWord64 x) <> B.word64BE (castDoubleToWord64 y)
  {-# INLINE pgEncode #-}
  pgOid _ = oidPoint
  {-# INLINE pgOid #-}

instance PgDecode PgPoint where
  pgDecode bs
    | BS.length bs /= 16 = Left $ "point: expected 16 bytes, got " <> show (BS.length bs)
    | otherwise =
        let x = castWord64ToDouble (decodeWord64BE bs 0)
            y = castWord64ToDouble (decodeWord64BE bs 8)
         in Right (PgPoint x y)
  {-# INLINE pgDecode #-}

decodeWord64BE :: ByteString -> Int -> Word64
decodeWord64BE bs off =
  let b0 = fromIntegral (BS.index bs off) :: Word64
      b1 = fromIntegral (BS.index bs (off + 1)) :: Word64
      b2 = fromIntegral (BS.index bs (off + 2)) :: Word64
      b3 = fromIntegral (BS.index bs (off + 3)) :: Word64
      b4 = fromIntegral (BS.index bs (off + 4)) :: Word64
      b5 = fromIntegral (BS.index bs (off + 5)) :: Word64
      b6 = fromIntegral (BS.index bs (off + 6)) :: Word64
      b7 = fromIntegral (BS.index bs (off + 7)) :: Word64
   in b0 `shiftL` 56 .|. b1 `shiftL` 48 .|. b2 `shiftL` 40 .|. b3 `shiftL` 32
      .|. b4 `shiftL` 24 .|. b5 `shiftL` 16 .|. b6 `shiftL` 8 .|. b7

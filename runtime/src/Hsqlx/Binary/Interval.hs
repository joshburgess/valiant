{-# OPTIONS_GHC -Wno-orphans #-}

-- | Binary encode/decode for PostgreSQL @interval@ type.
--
-- PG stores interval as:
--   8 bytes: microseconds (Int64)
--   4 bytes: days (Int32)
--   4 bytes: months (Int32)
-- Total: 16 bytes, big-endian.
module Hsqlx.Binary.Interval
  ( PgInterval (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32, Int64)
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import PgWire.Protocol.Oid (oidInterval)

-- | A PostgreSQL interval value.
data PgInterval = PgInterval
  { intervalMicroseconds :: {-# UNPACK #-} !Int64
  , intervalDays :: {-# UNPACK #-} !Int32
  , intervalMonths :: {-# UNPACK #-} !Int32
  }
  deriving stock (Show, Eq, Ord, Generic)

instance NFData PgInterval

instance PgEncode PgInterval where
  pgEncode (PgInterval micros days months) =
    LBS.toStrict . B.toLazyByteString $
      B.int64BE micros
        <> B.int32BE days
        <> B.int32BE months
  pgOid _ = oidInterval

instance PgDecode PgInterval where
  pgDecode bs
    | BS.length bs /= 16 = Left $ "interval: expected 16 bytes, got " <> show (BS.length bs)
    | otherwise = do
        micros <- decodeInt64BE bs 0
        days <- decodeInt32BE bs 8
        months <- decodeInt32BE bs 12
        Right (PgInterval micros days months)

-- Helpers -------------------------------------------------------------------

decodeInt32BE :: ByteString -> Int -> Either String Int32
decodeInt32BE bs off
  | off + 4 > BS.length bs = Left "interval: int32 out of bounds"
  | otherwise =
      let b0 = fromIntegral (BS.index bs off) :: Int32
          b1 = fromIntegral (BS.index bs (off + 1)) :: Int32
          b2 = fromIntegral (BS.index bs (off + 2)) :: Int32
          b3 = fromIntegral (BS.index bs (off + 3)) :: Int32
       in Right (b0 `shiftL` 24 .|. b1 `shiftL` 16 .|. b2 `shiftL` 8 .|. b3)

decodeInt64BE :: ByteString -> Int -> Either String Int64
decodeInt64BE bs off
  | off + 8 > BS.length bs = Left "interval: int64 out of bounds"
  | otherwise =
      let go !acc !i
            | i >= 8 = acc
            | otherwise = go (acc `shiftL` 8 .|. fromIntegral (BS.index bs (off + i))) (i + 1)
       in Right (go 0 0)

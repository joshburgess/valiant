{-# OPTIONS_GHC -Wno-orphans #-}

-- | Support for PostgreSQL @-infinity@ and @infinity@ timestamp/date values.
--
-- PostgreSQL allows @-infinity@ and @infinity@ as valid values for
-- @timestamp@, @timestamptz@, and @date@ columns. In binary format these
-- are represented as sentinel values:
--
--   * Timestamps: @INT64_MIN@ for @-infinity@, @INT64_MAX@ for @infinity@
--   * Dates: @INT32_MIN@ for @-infinity@, @INT32_MAX@ for @infinity@
--
-- The standard 'UTCTime', 'LocalTime', and 'Day' types cannot represent
-- infinity. This module provides 'Unbounded' as a wrapper:
--
-- @
-- findUser :: Statement Int32 (Int32, Text, Unbounded UTCTime)
-- findUser = queryFile \"users\/find_by_id.sql\"
-- @
module Valiant.Binary.Unbounded
  ( Unbounded (..)
  , finite
  , fromFinite
  ) where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Bits (shiftL, (.|.))
import Data.Int (Int32, Int64)
import Data.Time (Day, LocalTime, UTCTime)
import GHC.Generics (Generic)
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import PgWire.Protocol.Oid (oidDate, oidTimestamp, oidTimestamptz)
import Valiant.Binary.Decode ()
import Valiant.Binary.Encode ()

-- | A value that may be @-infinity@, a finite value, or @infinity@.
-- Used for PostgreSQL timestamp and date types that support infinity.
data Unbounded a
  = NegInfinity
  -- ^ @-infinity@
  | Finite !a
  -- ^ A regular finite value.
  | PosInfinity
  -- ^ @infinity@
  deriving stock (Show, Eq, Ord, Generic, Functor)

instance NFData a => NFData (Unbounded a)

-- | Wrap a value as 'Finite'.
finite :: a -> Unbounded a
finite = Finite
{-# INLINE finite #-}

-- | Extract the finite value, or 'Nothing' for infinity.
fromFinite :: Unbounded a -> Maybe a
fromFinite (Finite a) = Just a
fromFinite _ = Nothing
{-# INLINE fromFinite #-}

-- Sentinel values used by PostgreSQL binary format.
pgTimestampPosInf :: Int64
pgTimestampPosInf = 0x7FFFFFFFFFFFFFFF

pgTimestampNegInf :: Int64
pgTimestampNegInf = -0x8000000000000000

pgDatePosInf :: Int32
pgDatePosInf = 0x7FFFFFFF

pgDateNegInf :: Int32
pgDateNegInf = -0x80000000

------------------------------------------------------------------------
-- Unbounded UTCTime (timestamptz)
------------------------------------------------------------------------

instance PgEncode (Unbounded UTCTime) where
  pgEncode NegInfinity = encodeInt64BE pgTimestampNegInf
  pgEncode PosInfinity = encodeInt64BE pgTimestampPosInf
  pgEncode (Finite t) = pgEncode t
  {-# INLINE pgEncode #-}
  pgOid _ = oidTimestamptz
  {-# INLINE pgOid #-}

instance PgDecode (Unbounded UTCTime) where
  pgDecode bs = do
    micros <- decodeRawInt64 bs
    if micros == pgTimestampPosInf then Right PosInfinity
    else if micros == pgTimestampNegInf then Right NegInfinity
    else Finite <$> pgDecode bs
  {-# INLINE pgDecode #-}

------------------------------------------------------------------------
-- Unbounded LocalTime (timestamp)
------------------------------------------------------------------------

instance PgEncode (Unbounded LocalTime) where
  pgEncode NegInfinity = encodeInt64BE pgTimestampNegInf
  pgEncode PosInfinity = encodeInt64BE pgTimestampPosInf
  pgEncode (Finite t) = pgEncode t
  {-# INLINE pgEncode #-}
  pgOid _ = oidTimestamp
  {-# INLINE pgOid #-}

instance PgDecode (Unbounded LocalTime) where
  pgDecode bs = do
    micros <- decodeRawInt64 bs
    if micros == pgTimestampPosInf then Right PosInfinity
    else if micros == pgTimestampNegInf then Right NegInfinity
    else Finite <$> pgDecode bs
  {-# INLINE pgDecode #-}

------------------------------------------------------------------------
-- Unbounded Day (date)
------------------------------------------------------------------------

instance PgEncode (Unbounded Day) where
  pgEncode NegInfinity = encodeInt32BE pgDateNegInf
  pgEncode PosInfinity = encodeInt32BE pgDatePosInf
  pgEncode (Finite d) = pgEncode d
  {-# INLINE pgEncode #-}
  pgOid _ = oidDate
  {-# INLINE pgOid #-}

instance PgDecode (Unbounded Day) where
  pgDecode bs = do
    days <- decodeRawInt32 bs
    if days == pgDatePosInf then Right PosInfinity
    else if days == pgDateNegInf then Right NegInfinity
    else Finite <$> pgDecode bs
  {-# INLINE pgDecode #-}

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

decodeRawInt64 :: ByteString -> Either String Int64
decodeRawInt64 bs
  | BS.length bs /= 8 = Left $ "Unbounded: expected 8 bytes, got " <> show (BS.length bs)
  | otherwise =
      let b0 = fromIntegral (BS.index bs 0) :: Int64
          b1 = fromIntegral (BS.index bs 1) :: Int64
          b2 = fromIntegral (BS.index bs 2) :: Int64
          b3 = fromIntegral (BS.index bs 3) :: Int64
          b4 = fromIntegral (BS.index bs 4) :: Int64
          b5 = fromIntegral (BS.index bs 5) :: Int64
          b6 = fromIntegral (BS.index bs 6) :: Int64
          b7 = fromIntegral (BS.index bs 7) :: Int64
       in Right $ b0 `shiftL` 56 .|. b1 `shiftL` 48 .|. b2 `shiftL` 40 .|. b3 `shiftL` 32
                  .|. b4 `shiftL` 24 .|. b5 `shiftL` 16 .|. b6 `shiftL` 8 .|. b7

decodeRawInt32 :: ByteString -> Either String Int32
decodeRawInt32 bs
  | BS.length bs /= 4 = Left $ "Unbounded: expected 4 bytes, got " <> show (BS.length bs)
  | otherwise =
      let b0 = fromIntegral (BS.index bs 0) :: Int32
          b1 = fromIntegral (BS.index bs 1) :: Int32
          b2 = fromIntegral (BS.index bs 2) :: Int32
          b3 = fromIntegral (BS.index bs 3) :: Int32
       in Right $ b0 `shiftL` 24 .|. b1 `shiftL` 16 .|. b2 `shiftL` 8 .|. b3

encodeInt64BE :: Int64 -> ByteString
encodeInt64BE = pgEncode

encodeInt32BE :: Int32 -> ByteString
encodeInt32BE = pgEncode

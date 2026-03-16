{-# OPTIONS_GHC -Wno-orphans #-}

module Hsqlx.Binary.Encode
  () where

import Data.Bits (unsafeShiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal (unsafeCreate)
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time
  ( Day
  , LocalTime (..)
  , TimeOfDay (..)
  , UTCTime (..)
  , diffTimeToPicoseconds
  , fromGregorian
  , timeOfDayToTime
  , toModifiedJulianDay
  )
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word8, Word32, Word64)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import GHC.Float (castFloatToWord32, castDoubleToWord64)
import PgWire.Binary.Types (PgEncode (..))
import PgWire.Protocol.Oid

-- PG epoch: 2000-01-01 00:00:00 UTC
pgEpochOffsetMicros :: Int64
pgEpochOffsetMicros = 946684800000000
{-# INLINE pgEpochOffsetMicros #-}

pgEpochDay :: Integer
pgEpochDay = toModifiedJulianDay (fromGregorian 2000 1 1)
{-# INLINE pgEpochDay #-}

-- Direct byte writes --------------------------------------------------------
-- These avoid Builder → LazyByteString → toStrict overhead for fixed-size
-- types. A single unsafeCreate + poke is ~3x faster than the Builder path
-- for 2-8 byte values.

int16BE :: Int16 -> ByteString
int16BE n = unsafeCreate 2 $ \p -> pokeInt16BE p 0 n
{-# INLINE int16BE #-}

int32BE :: Int32 -> ByteString
int32BE n = unsafeCreate 4 $ \p -> pokeInt32BE p 0 n
{-# INLINE int32BE #-}

int64BE :: Int64 -> ByteString
int64BE n = unsafeCreate 8 $ \p -> pokeInt64BE p 0 n
{-# INLINE int64BE #-}

floatBE :: Float -> ByteString
floatBE f = let !w = castFloatToWord32 f in unsafeCreate 4 $ \p -> pokeWord32BE p 0 w
{-# INLINE floatBE #-}

doubleBE :: Double -> ByteString
doubleBE d = let !w = castDoubleToWord64 d in unsafeCreate 8 $ \p -> pokeWord64BE p 0 w
{-# INLINE doubleBE #-}

-- Poke helpers — write big-endian at an offset into a Ptr
pokeInt16BE :: Ptr Word8 -> Int -> Int16 -> IO ()
pokeInt16BE p off n = do
  pokeByteOff p off       (fromIntegral (n `unsafeShiftR` 8) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral n :: Word8)
{-# INLINE pokeInt16BE #-}

pokeInt32BE :: Ptr Word8 -> Int -> Int32 -> IO ()
pokeInt32BE p off n = do
  pokeByteOff p off       (fromIntegral (n `unsafeShiftR` 24) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral (n `unsafeShiftR` 16) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral (n `unsafeShiftR` 8) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral n :: Word8)
{-# INLINE pokeInt32BE #-}

pokeInt64BE :: Ptr Word8 -> Int -> Int64 -> IO ()
pokeInt64BE p off n = do
  pokeByteOff p off       (fromIntegral (n `unsafeShiftR` 56) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral (n `unsafeShiftR` 48) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral (n `unsafeShiftR` 40) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral (n `unsafeShiftR` 32) :: Word8)
  pokeByteOff p (off + 4) (fromIntegral (n `unsafeShiftR` 24) :: Word8)
  pokeByteOff p (off + 5) (fromIntegral (n `unsafeShiftR` 16) :: Word8)
  pokeByteOff p (off + 6) (fromIntegral (n `unsafeShiftR` 8) :: Word8)
  pokeByteOff p (off + 7) (fromIntegral n :: Word8)
{-# INLINE pokeInt64BE #-}

pokeWord32BE :: Ptr Word8 -> Int -> Word32 -> IO ()
pokeWord32BE p off n = do
  pokeByteOff p off       (fromIntegral (n `unsafeShiftR` 24) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral (n `unsafeShiftR` 16) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral (n `unsafeShiftR` 8) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral n :: Word8)
{-# INLINE pokeWord32BE #-}

pokeWord64BE :: Ptr Word8 -> Int -> Word64 -> IO ()
pokeWord64BE p off n = do
  pokeByteOff p off       (fromIntegral (n `unsafeShiftR` 56) :: Word8)
  pokeByteOff p (off + 1) (fromIntegral (n `unsafeShiftR` 48) :: Word8)
  pokeByteOff p (off + 2) (fromIntegral (n `unsafeShiftR` 40) :: Word8)
  pokeByteOff p (off + 3) (fromIntegral (n `unsafeShiftR` 32) :: Word8)
  pokeByteOff p (off + 4) (fromIntegral (n `unsafeShiftR` 24) :: Word8)
  pokeByteOff p (off + 5) (fromIntegral (n `unsafeShiftR` 16) :: Word8)
  pokeByteOff p (off + 6) (fromIntegral (n `unsafeShiftR` 8) :: Word8)
  pokeByteOff p (off + 7) (fromIntegral n :: Word8)
{-# INLINE pokeWord64BE #-}

-- Instances ---------------------------------------------------------------

instance PgEncode Bool where
  pgEncode True = BS.singleton 1
  pgEncode False = BS.singleton 0
  {-# INLINE pgEncode #-}
  pgOid _ = oidBool
  {-# INLINE pgOid #-}

instance PgEncode Int16 where
  pgEncode = int16BE
  {-# INLINE pgEncode #-}
  pgOid _ = oidInt2
  {-# INLINE pgOid #-}

instance PgEncode Int32 where
  pgEncode = int32BE
  {-# INLINE pgEncode #-}
  pgOid _ = oidInt4
  {-# INLINE pgOid #-}

instance PgEncode Int64 where
  pgEncode = int64BE
  {-# INLINE pgEncode #-}
  pgOid _ = oidInt8
  {-# INLINE pgOid #-}

instance PgEncode Float where
  pgEncode = floatBE
  {-# INLINE pgEncode #-}
  pgOid _ = oidFloat4
  {-# INLINE pgOid #-}

instance PgEncode Double where
  pgEncode = doubleBE
  {-# INLINE pgEncode #-}
  pgOid _ = oidFloat8
  {-# INLINE pgOid #-}

instance PgEncode Text where
  pgEncode = TE.encodeUtf8
  {-# INLINE pgEncode #-}
  pgOid _ = oidText
  {-# INLINE pgOid #-}

instance PgEncode ByteString where
  pgEncode = id
  {-# INLINE pgEncode #-}
  pgOid _ = oidBytea
  {-# INLINE pgOid #-}

instance PgEncode UTCTime where
  pgEncode t =
    let !unixMicros = round (utcTimeToPOSIXSeconds t * 1000000) :: Int64
        !pgMicros = unixMicros - pgEpochOffsetMicros
     in int64BE pgMicros
  {-# INLINE pgEncode #-}
  pgOid _ = oidTimestamptz
  {-# INLINE pgOid #-}

instance PgEncode Day where
  pgEncode d =
    let !daysSincePgEpoch = toModifiedJulianDay d - pgEpochDay
     in int32BE (fromIntegral daysSincePgEpoch)
  {-# INLINE pgEncode #-}
  pgOid _ = oidDate
  {-# INLINE pgOid #-}

instance PgEncode TimeOfDay where
  pgEncode tod =
    let !picos = diffTimeToPicoseconds (timeOfDayToTime tod)
        !micros = picos `div` 1000000
     in int64BE (fromIntegral micros)
  {-# INLINE pgEncode #-}
  pgOid _ = oidTime
  {-# INLINE pgOid #-}

instance PgEncode LocalTime where
  pgEncode (LocalTime d tod) =
    let !daysSincePgEpoch = toModifiedJulianDay d - pgEpochDay
        !picos = diffTimeToPicoseconds (timeOfDayToTime tod)
        !microsSinceMidnight = picos `div` 1000000
        !totalMicros = fromIntegral daysSincePgEpoch * 86400000000 + fromIntegral microsSinceMidnight
     in int64BE totalMicros
  {-# INLINE pgEncode #-}
  pgOid _ = oidTimestamp
  {-# INLINE pgOid #-}

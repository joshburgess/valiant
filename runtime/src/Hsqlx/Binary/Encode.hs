{-# OPTIONS_GHC -Wno-orphans #-}

module Hsqlx.Binary.Encode
  () where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
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
import PgWire.Binary.Types (PgEncode (..))
import PgWire.Protocol.Oid

-- PG epoch: 2000-01-01 00:00:00 UTC
-- Microseconds between Unix epoch (1970-01-01) and PG epoch (2000-01-01)
pgEpochOffsetMicros :: Int64
pgEpochOffsetMicros = 946684800000000
{-# INLINE pgEpochOffsetMicros #-}

-- PG date epoch: 2000-01-01 as Modified Julian Day
pgEpochDay :: Integer
pgEpochDay = toModifiedJulianDay (fromGregorian 2000 1 1)
{-# INLINE pgEpochDay #-}

-- Helpers -----------------------------------------------------------------

int16BE :: Int16 -> ByteString
int16BE = LBS.toStrict . B.toLazyByteString . B.int16BE
{-# INLINE int16BE #-}

int32BE :: Int32 -> ByteString
int32BE = LBS.toStrict . B.toLazyByteString . B.int32BE
{-# INLINE int32BE #-}

int64BE :: Int64 -> ByteString
int64BE = LBS.toStrict . B.toLazyByteString . B.int64BE
{-# INLINE int64BE #-}

floatBE :: Float -> ByteString
floatBE = LBS.toStrict . B.toLazyByteString . B.floatBE
{-# INLINE floatBE #-}

doubleBE :: Double -> ByteString
doubleBE = LBS.toStrict . B.toLazyByteString . B.doubleBE
{-# INLINE doubleBE #-}

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
    let unixMicros = round (utcTimeToPOSIXSeconds t * 1000000) :: Int64
        pgMicros = unixMicros - pgEpochOffsetMicros
     in int64BE pgMicros
  {-# INLINE pgEncode #-}
  pgOid _ = oidTimestamptz
  {-# INLINE pgOid #-}

instance PgEncode Day where
  pgEncode d =
    let daysSincePgEpoch = toModifiedJulianDay d - pgEpochDay
     in int32BE (fromIntegral daysSincePgEpoch)
  {-# INLINE pgEncode #-}
  pgOid _ = oidDate
  {-# INLINE pgOid #-}

instance PgEncode TimeOfDay where
  pgEncode tod =
    let picos = diffTimeToPicoseconds (timeOfDayToTime tod)
        micros = picos `div` 1000000
     in int64BE (fromIntegral micros)
  {-# INLINE pgEncode #-}
  pgOid _ = oidTime
  {-# INLINE pgOid #-}

instance PgEncode LocalTime where
  pgEncode (LocalTime d tod) =
    let daysSincePgEpoch = toModifiedJulianDay d - pgEpochDay
        picos = diffTimeToPicoseconds (timeOfDayToTime tod)
        microsSinceMidnight = picos `div` 1000000
        totalMicros = fromIntegral daysSincePgEpoch * 86400000000 + fromIntegral microsSinceMidnight
     in int64BE totalMicros
  {-# INLINE pgEncode #-}
  pgOid _ = oidTimestamp
  {-# INLINE pgOid #-}

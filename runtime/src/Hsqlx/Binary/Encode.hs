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
import Hsqlx.Binary.Types (PgEncode (..))
import Hsqlx.Protocol.Oid

-- PG epoch: 2000-01-01 00:00:00 UTC
-- Microseconds between Unix epoch (1970-01-01) and PG epoch (2000-01-01)
pgEpochOffsetMicros :: Int64
pgEpochOffsetMicros = 946684800000000

-- PG date epoch: 2000-01-01 as Modified Julian Day
pgEpochDay :: Integer
pgEpochDay = toModifiedJulianDay (fromGregorian 2000 1 1)

-- Helpers -----------------------------------------------------------------

int16BE :: Int16 -> ByteString
int16BE = LBS.toStrict . B.toLazyByteString . B.int16BE

int32BE :: Int32 -> ByteString
int32BE = LBS.toStrict . B.toLazyByteString . B.int32BE

int64BE :: Int64 -> ByteString
int64BE = LBS.toStrict . B.toLazyByteString . B.int64BE

floatBE :: Float -> ByteString
floatBE = LBS.toStrict . B.toLazyByteString . B.floatBE

doubleBE :: Double -> ByteString
doubleBE = LBS.toStrict . B.toLazyByteString . B.doubleBE

-- Instances ---------------------------------------------------------------

instance PgEncode Bool where
  pgEncode True = BS.singleton 1
  pgEncode False = BS.singleton 0
  pgOid _ = oidBool

instance PgEncode Int16 where
  pgEncode = int16BE
  pgOid _ = oidInt2

instance PgEncode Int32 where
  pgEncode = int32BE
  pgOid _ = oidInt4

instance PgEncode Int64 where
  pgEncode = int64BE
  pgOid _ = oidInt8

instance PgEncode Float where
  pgEncode = floatBE
  pgOid _ = oidFloat4

instance PgEncode Double where
  pgEncode = doubleBE
  pgOid _ = oidFloat8

instance PgEncode Text where
  pgEncode = TE.encodeUtf8
  pgOid _ = oidText

instance PgEncode ByteString where
  pgEncode = id
  pgOid _ = oidBytea

instance PgEncode UTCTime where
  pgEncode t =
    let unixMicros = round (utcTimeToPOSIXSeconds t * 1000000) :: Int64
        pgMicros = unixMicros - pgEpochOffsetMicros
     in int64BE pgMicros
  pgOid _ = oidTimestamptz

instance PgEncode Day where
  pgEncode d =
    let daysSincePgEpoch = toModifiedJulianDay d - pgEpochDay
     in int32BE (fromIntegral daysSincePgEpoch)
  pgOid _ = oidDate

instance PgEncode TimeOfDay where
  pgEncode tod =
    let picos = diffTimeToPicoseconds (timeOfDayToTime tod)
        micros = picos `div` 1000000
     in int64BE (fromIntegral micros)
  pgOid _ = oidTime

instance PgEncode LocalTime where
  pgEncode (LocalTime d tod) =
    let daysSincePgEpoch = toModifiedJulianDay d - pgEpochDay
        picos = diffTimeToPicoseconds (timeOfDayToTime tod)
        microsSinceMidnight = picos `div` 1000000
        totalMicros = fromIntegral daysSincePgEpoch * 86400000000 + fromIntegral microsSinceMidnight
     in int64BE totalMicros
  pgOid _ = oidTimestamp

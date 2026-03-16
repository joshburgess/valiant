{-# OPTIONS_GHC -Wno-orphans #-}

module Hsqlx.Binary.Decode
  () where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time
  ( Day
  , LocalTime (..)
  , TimeOfDay
  , UTCTime (..)
  , fromGregorian
  , picosecondsToDiffTime
  , timeToTimeOfDay
  )
import Data.Time.Calendar (addDays)
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Hsqlx.Binary.Types (PgDecode (..))

-- PG epoch offset from Unix epoch in seconds
pgEpochOffsetSeconds :: Int64
pgEpochOffsetSeconds = 946684800

pgEpochDay :: Day
pgEpochDay = fromGregorian 2000 1 1

-- Helpers -----------------------------------------------------------------

decodeInt16BE :: ByteString -> Either String Int16
decodeInt16BE bs
  | BS.length bs /= 2 = Left $ "int16: expected 2 bytes, got " <> show (BS.length bs)
  | otherwise =
      let b0 = fromIntegral (BS.index bs 0) :: Int16
          b1 = fromIntegral (BS.index bs 1) :: Int16
       in Right (b0 `shiftL` 8 .|. b1)

decodeInt32BE :: ByteString -> Either String Int32
decodeInt32BE bs
  | BS.length bs /= 4 = Left $ "int32: expected 4 bytes, got " <> show (BS.length bs)
  | otherwise =
      let b0 = fromIntegral (BS.index bs 0) :: Int32
          b1 = fromIntegral (BS.index bs 1) :: Int32
          b2 = fromIntegral (BS.index bs 2) :: Int32
          b3 = fromIntegral (BS.index bs 3) :: Int32
       in Right (b0 `shiftL` 24 .|. b1 `shiftL` 16 .|. b2 `shiftL` 8 .|. b3)

decodeInt64BE :: ByteString -> Either String Int64
decodeInt64BE bs
  | BS.length bs /= 8 = Left $ "int64: expected 8 bytes, got " <> show (BS.length bs)
  | otherwise =
      let go acc i
            | i >= 8 = acc
            | otherwise = go (acc `shiftL` 8 .|. fromIntegral (BS.index bs i)) (i + 1)
       in Right (go 0 0)

decodeWord32BE :: ByteString -> Either String Word32
decodeWord32BE bs = fromIntegral <$> decodeInt32BE bs

decodeWord64BE :: ByteString -> Either String Word64
decodeWord64BE bs = fromIntegral <$> decodeInt64BE bs

-- Instances ---------------------------------------------------------------

instance PgDecode Bool where
  pgDecode bs
    | BS.length bs /= 1 = Left "bool: expected 1 byte"
    | otherwise = Right (BS.index bs 0 /= 0)

instance PgDecode Int16 where
  pgDecode = decodeInt16BE

instance PgDecode Int32 where
  pgDecode = decodeInt32BE

instance PgDecode Int64 where
  pgDecode = decodeInt64BE

instance PgDecode Float where
  pgDecode bs = castWord32ToFloat <$> decodeWord32BE bs

instance PgDecode Double where
  pgDecode bs = castWord64ToDouble <$> decodeWord64BE bs

instance PgDecode Text where
  pgDecode bs = case TE.decodeUtf8' bs of
    Left err -> Left ("text decode: " <> show err)
    Right t -> Right t

instance PgDecode ByteString where
  pgDecode = Right

instance PgDecode UTCTime where
  pgDecode bs = do
    pgMicros <- decodeInt64BE bs
    -- PG stores microseconds since 2000-01-01 00:00:00 UTC
    -- Convert to UTCTime by adding to PG epoch
    let totalMicros = pgMicros + pgEpochOffsetSeconds * 1000000
        (days, remainMicros) = totalMicros `divMod` 86400000000
        unixEpoch = fromGregorian 1970 1 1
        day = addDays (fromIntegral days) unixEpoch
        picos = fromIntegral remainMicros * 1000000
    Right (UTCTime day (picosecondsToDiffTime picos))

instance PgDecode Day where
  pgDecode bs = do
    days <- decodeInt32BE bs
    Right (addDays (fromIntegral days) pgEpochDay)

instance PgDecode TimeOfDay where
  pgDecode bs = do
    micros <- decodeInt64BE bs
    let picos = fromIntegral micros * 1000000
    Right (timeToTimeOfDay (picosecondsToDiffTime picos))

instance PgDecode LocalTime where
  pgDecode bs = do
    totalMicros <- decodeInt64BE bs
    let (days, remainMicros) = totalMicros `divMod` 86400000000
        day = addDays (fromIntegral days) pgEpochDay
        picos = fromIntegral remainMicros * 1000000
        tod = timeToTimeOfDay (picosecondsToDiffTime picos)
    Right (LocalTime day tod)

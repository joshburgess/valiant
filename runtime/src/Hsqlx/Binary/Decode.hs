{-# OPTIONS_GHC -Wno-orphans #-}

module Hsqlx.Binary.Decode
  () where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BU
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
import PgWire.Binary.Types (PgDecode (..))

-- PG epoch offset from Unix epoch in seconds
pgEpochOffsetSeconds :: Int64
pgEpochOffsetSeconds = 946684800
{-# INLINE pgEpochOffsetSeconds #-}

pgEpochDay :: Day
pgEpochDay = fromGregorian 2000 1 1
{-# INLINE pgEpochDay #-}

-- Helpers -----------------------------------------------------------------

decodeInt16BE :: ByteString -> Either String Int16
decodeInt16BE bs
  | BS.length bs /= 2 = Left $ "int16: expected 2 bytes, got " <> show (BS.length bs)
  | otherwise =
      let b0 = fromIntegral (BU.unsafeIndex bs 0) :: Int16
          b1 = fromIntegral (BU.unsafeIndex bs 1) :: Int16
       in Right (b0 `shiftL` 8 .|. b1)
{-# INLINE decodeInt16BE #-}

decodeInt32BE :: ByteString -> Either String Int32
decodeInt32BE bs
  | BS.length bs /= 4 = Left $ "int32: expected 4 bytes, got " <> show (BS.length bs)
  | otherwise =
      let b0 = fromIntegral (BU.unsafeIndex bs 0) :: Int32
          b1 = fromIntegral (BU.unsafeIndex bs 1) :: Int32
          b2 = fromIntegral (BU.unsafeIndex bs 2) :: Int32
          b3 = fromIntegral (BU.unsafeIndex bs 3) :: Int32
       in Right (b0 `shiftL` 24 .|. b1 `shiftL` 16 .|. b2 `shiftL` 8 .|. b3)
{-# INLINE decodeInt32BE #-}

decodeInt64BE :: ByteString -> Either String Int64
decodeInt64BE bs
  | BS.length bs /= 8 = Left $ "int64: expected 8 bytes, got " <> show (BS.length bs)
  | otherwise =
      let !b0 = fromIntegral (BU.unsafeIndex bs 0) :: Int64
          !b1 = fromIntegral (BU.unsafeIndex bs 1) :: Int64
          !b2 = fromIntegral (BU.unsafeIndex bs 2) :: Int64
          !b3 = fromIntegral (BU.unsafeIndex bs 3) :: Int64
          !b4 = fromIntegral (BU.unsafeIndex bs 4) :: Int64
          !b5 = fromIntegral (BU.unsafeIndex bs 5) :: Int64
          !b6 = fromIntegral (BU.unsafeIndex bs 6) :: Int64
          !b7 = fromIntegral (BU.unsafeIndex bs 7) :: Int64
       in Right (b0 `shiftL` 56 .|. b1 `shiftL` 48 .|. b2 `shiftL` 40 .|. b3 `shiftL` 32
            .|. b4 `shiftL` 24 .|. b5 `shiftL` 16 .|. b6 `shiftL` 8 .|. b7)
{-# INLINE decodeInt64BE #-}

decodeWord32BE :: ByteString -> Either String Word32
decodeWord32BE bs = fromIntegral <$> decodeInt32BE bs
{-# INLINE decodeWord32BE #-}

decodeWord64BE :: ByteString -> Either String Word64
decodeWord64BE bs = fromIntegral <$> decodeInt64BE bs
{-# INLINE decodeWord64BE #-}

-- Instances ---------------------------------------------------------------

instance PgDecode Bool where
  pgDecode bs
    | BS.length bs /= 1 = Left "bool: expected 1 byte"
    | otherwise = Right (BU.unsafeIndex bs 0 /= 0)
  {-# INLINE pgDecode #-}

instance PgDecode Int16 where
  pgDecode = decodeInt16BE
  {-# INLINE pgDecode #-}

instance PgDecode Int32 where
  pgDecode = decodeInt32BE
  {-# INLINE pgDecode #-}

instance PgDecode Int64 where
  pgDecode = decodeInt64BE
  {-# INLINE pgDecode #-}

instance PgDecode Float where
  pgDecode bs = castWord32ToFloat <$> decodeWord32BE bs
  {-# INLINE pgDecode #-}

instance PgDecode Double where
  pgDecode bs = castWord64ToDouble <$> decodeWord64BE bs
  {-# INLINE pgDecode #-}

instance PgDecode Text where
  pgDecode bs = case TE.decodeUtf8' bs of
    Left err -> Left ("text decode: " <> show err)
    Right t -> Right t
  {-# INLINE pgDecode #-}

instance PgDecode ByteString where
  pgDecode = Right
  {-# INLINE pgDecode #-}

instance PgDecode UTCTime where
  pgDecode bs = do
    pgMicros <- decodeInt64BE bs
    let totalMicros = pgMicros + pgEpochOffsetSeconds * 1000000
        (days, remainMicros) = totalMicros `divMod` 86400000000
        unixEpoch = fromGregorian 1970 1 1
        day = addDays (fromIntegral days) unixEpoch
        picos = fromIntegral remainMicros * 1000000
    Right (UTCTime day (picosecondsToDiffTime picos))
  {-# INLINE pgDecode #-}

instance PgDecode Day where
  pgDecode bs = do
    days <- decodeInt32BE bs
    Right (addDays (fromIntegral days) pgEpochDay)
  {-# INLINE pgDecode #-}

instance PgDecode TimeOfDay where
  pgDecode bs = do
    micros <- decodeInt64BE bs
    let picos = fromIntegral micros * 1000000
    Right (timeToTimeOfDay (picosecondsToDiffTime picos))
  {-# INLINE pgDecode #-}

instance PgDecode LocalTime where
  pgDecode bs = do
    totalMicros <- decodeInt64BE bs
    let (days, remainMicros) = totalMicros `divMod` 86400000000
        day = addDays (fromIntegral days) pgEpochDay
        picos = fromIntegral remainMicros * 1000000
        tod = timeToTimeOfDay (picosecondsToDiffTime picos)
    Right (LocalTime day tod)
  {-# INLINE pgDecode #-}

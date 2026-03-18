module BenchCodec (benchmarks) where

import Criterion.Main
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
  ( Day
  , LocalTime (..)
  , TimeOfDay (..)
  , UTCTime (..)
  , fromGregorian
  , secondsToDiffTime
  )
import Data.Vector qualified as V
import Hsqlx.Binary.Array (pgDecodeArray, pgEncodeArray)
import Hsqlx.Binary.Decode ()
import Hsqlx.Binary.Encode ()
import Data.Time (TimeZone, minutesToTimeZone, ZonedTime (..), utcToZonedTime)
import Hsqlx.Binary.HStore (PgHStore (..), hstoreFromList)
import Hsqlx.Binary.Inet (PgInet, ipv4, ipv6Host)
import Hsqlx.Binary.Interval (PgInterval (..))
import Hsqlx.Binary.MacAddr (PgMacAddr, macAddr)
import Hsqlx.Binary.Point (PgPoint (..))
import Hsqlx.Binary.Scientific ()
import Hsqlx.Binary.Unbounded (Unbounded (..))
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import PgWire.Protocol.Oid

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "encode"
    [ bench "Bool"        $ nf pgEncode True
    , bench "Int16"       $ nf pgEncode (12345 :: Int16)
    , bench "Int32"       $ nf pgEncode (1234567 :: Int32)
    , bench "Int64"       $ nf pgEncode (123456789012 :: Int64)
    , bench "Float"       $ nf pgEncode (3.14 :: Float)
    , bench "Double"      $ nf pgEncode (3.14159265358979 :: Double)
    , bench "Text/short"  $ nf pgEncode ("hello" :: Text)
    , bench "Text/100"    $ nf pgEncode (T.replicate 100 "a" :: Text)
    , bench "Text/10000"  $ nf pgEncode (T.replicate 10000 "x" :: Text)
    , bench "ByteString/100"  $ nf pgEncode (BS.replicate 100 42)
    , bench "ByteString/10000" $ nf pgEncode (BS.replicate 10000 42)
    , bench "Day"         $ nf pgEncode (fromGregorian 2024 6 15)
    , bench "TimeOfDay"   $ nf pgEncode (TimeOfDay 14 30 45)
    , bench "UTCTime"     $ nf pgEncode sampleUTCTime
    , bench "LocalTime"   $ nf pgEncode sampleLocalTime
    , bench "Scientific"  $ nf pgEncode (scientific 314159 (-5))
    , bench "PgInterval"  $ nf pgEncode (PgInterval 3600000000 30 12)
    , bench "PgInet/v4"  $ nf pgEncode sampleInet4
    , bench "PgInet/v6"  $ nf pgEncode sampleInet6
    , bench "PgMacAddr"  $ nf pgEncode sampleMacAddr
    , bench "PgPoint"    $ nf pgEncode (PgPoint 1.5 2.5)
    , bench "PgHStore/5" $ nf pgEncode sampleHStore5
    , bench "PgHStore/50" $ nf pgEncode sampleHStore50
    , bench "ZonedTime"  $ nf pgEncode sampleZonedTime
    , bench "(TimeOfDay,TimeZone)" $ nf pgEncode sampleTimetz
    , bench "Unbounded UTCTime/finite" $ nf pgEncode (Finite sampleUTCTime)
    , bench "Unbounded UTCTime/inf"    $ nf pgEncode (PosInfinity :: Unbounded UTCTime)
    ]
  , bgroup "decode"
    [ bench "Bool"        $ nf (pgDecode @Bool)        (pgEncode True)
    , bench "Int16"       $ nf (pgDecode @Int16)       (pgEncode (12345 :: Int16))
    , bench "Int32"       $ nf (pgDecode @Int32)       (pgEncode (1234567 :: Int32))
    , bench "Int64"       $ nf (pgDecode @Int64)       (pgEncode (123456789012 :: Int64))
    , bench "Float"       $ nf (pgDecode @Float)       (pgEncode (3.14 :: Float))
    , bench "Double"      $ nf (pgDecode @Double)      (pgEncode (3.14159265358979 :: Double))
    , bench "Text/short"  $ nf (pgDecode @Text)        (pgEncode ("hello" :: Text))
    , bench "Text/100"    $ nf (pgDecode @Text)        (pgEncode (T.replicate 100 "a"))
    , bench "Day"         $ nf (pgDecode @Day)         (pgEncode (fromGregorian 2024 6 15))
    , bench "UTCTime"     $ nf (pgDecode @UTCTime)     (pgEncode sampleUTCTime)
    , bench "Scientific"  $ nf (pgDecode @Scientific)  encodedScientific
    , bench "PgInterval"  $ nf (pgDecode @PgInterval)  (pgEncode (PgInterval 3600000000 30 12))
    , bench "PgInet/v4"  $ nf (pgDecode @PgInet)      (pgEncode sampleInet4)
    , bench "PgMacAddr"  $ nf (pgDecode @PgMacAddr)    (pgEncode sampleMacAddr)
    , bench "PgPoint"    $ nf (pgDecode @PgPoint)      (pgEncode (PgPoint 1.5 2.5))
    , bench "PgHStore/5" $ nf (pgDecode @PgHStore)     (pgEncode sampleHStore5)
    , bench "PgHStore/50" $ nf (pgDecode @PgHStore)    (pgEncode sampleHStore50)
    , bench "ZonedTime"  $ nf (pgDecode @ZonedTime)    (pgEncode sampleZonedTime)
    , bench "(TimeOfDay,TimeZone)" $ nf (pgDecode @(TimeOfDay, TimeZone)) (pgEncode sampleTimetz)
    , bench "Unbounded UTCTime" $ nf (pgDecode @(Unbounded UTCTime)) (pgEncode (Finite sampleUTCTime))
    ]
  , bgroup "array/encode"
    [ bench "Int32/10"    $ nf (pgEncodeArray oidInt4) (V.fromList [1..10 :: Int32])
    , bench "Int32/100"   $ nf (pgEncodeArray oidInt4) (V.fromList [1..100 :: Int32])
    , bench "Int32/1000"  $ nf (pgEncodeArray oidInt4) (V.fromList [1..1000 :: Int32])
    , bench "Text/100"    $ nf (pgEncodeArray oidText) (V.replicate 100 ("hello" :: Text))
    ]
  , bgroup "array/decode"
    [ bench "Int32/10"    $ nf (pgDecodeArray @Int32) encodedArr10
    , bench "Int32/100"   $ nf (pgDecodeArray @Int32) encodedArr100
    , bench "Int32/1000"  $ nf (pgDecodeArray @Int32) encodedArr1000
    , bench "Text/100"    $ nf (pgDecodeArray @Text)  encodedTextArr100
    ]
  ]

sampleUTCTime :: UTCTime
sampleUTCTime = UTCTime (fromGregorian 2024 6 15) (secondsToDiffTime (12 * 3600 + 30 * 60))

sampleLocalTime :: LocalTime
sampleLocalTime = LocalTime (fromGregorian 2024 6 15) (TimeOfDay 14 30 0)

encodedScientific :: BS.ByteString
encodedScientific = pgEncode (scientific 314159 (-5))

encodedArr10 :: BS.ByteString
encodedArr10 = pgEncodeArray oidInt4 (V.fromList [1..10 :: Int32])

encodedArr100 :: BS.ByteString
encodedArr100 = pgEncodeArray oidInt4 (V.fromList [1..100 :: Int32])

encodedArr1000 :: BS.ByteString
encodedArr1000 = pgEncodeArray oidInt4 (V.fromList [1..1000 :: Int32])

encodedTextArr100 :: BS.ByteString
encodedTextArr100 = pgEncodeArray oidText (V.replicate 100 ("hello" :: Text))

sampleInet4 :: PgInet
sampleInet4 = ipv4 192 168 1 0 24

sampleInet6 :: PgInet
sampleInet6 = ipv6Host (BS.pack [0x20,0x01,0x0d,0xb8,0,0,0,0,0,0,0,0,0,0,0,1])

sampleMacAddr :: PgMacAddr
sampleMacAddr = macAddr 0x08 0x00 0x2b 0x01 0x02 0x03

sampleHStore5 :: PgHStore
sampleHStore5 = hstoreFromList
  [("k1","v1"),("k2","v2"),("k3","v3"),("k4","v4"),("k5","v5")]

sampleHStore50 :: PgHStore
sampleHStore50 = hstoreFromList
  [("key_" <> T.pack (show i), "value_" <> T.pack (show i)) | i <- [1::Int .. 50]]

sampleZonedTime :: ZonedTime
sampleZonedTime = utcToZonedTime (minutesToTimeZone 0) sampleUTCTime

sampleTimetz :: (TimeOfDay, TimeZone)
sampleTimetz = (TimeOfDay 14 30 45, minutesToTimeZone (-300))

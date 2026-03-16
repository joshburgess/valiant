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
import Hsqlx.Binary.Interval (PgInterval (..))
import Hsqlx.Binary.Scientific ()
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

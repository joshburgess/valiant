module Hsqlx.Binary.PropertySpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64)
import Data.Scientific (Scientific, scientific)
import Data.Text qualified as T
import Data.Time
  ( Day
  , LocalTime (..)
  , TimeOfDay (..)
  , UTCTime (..)
  , fromGregorian
  , picosecondsToDiffTime
  )
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import Data.Vector qualified as V
import Data.Word (Word8, Word32)
import Hsqlx.Binary.Array (pgDecodeArray, pgEncodeArray)
import Hsqlx.Binary.Decode ()
import Hsqlx.Binary.Encode ()
import Hsqlx.Binary.Interval (PgInterval (..))
import Hsqlx.Binary.JSON ()
import Hsqlx.Binary.Range (PgRange (..), RangeBound (..), pgDecodeRange, pgEncodeRange)
import Hsqlx.Binary.Scientific ()
import Hsqlx.Binary.UUID ()
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import PgWire.Protocol.Oid
import Test.Hspec
import Test.QuickCheck

-- | Helper: run a QuickCheck property as an hspec test.
prop :: (Testable p) => String -> p -> Spec
prop name = it name . property

spec :: Spec
spec = do
  describe "Bool round-trip property" $
    prop "encode/decode is identity" $ \(b :: Bool) ->
      pgDecode (pgEncode b) === Right b

  describe "Int16 round-trip property" $
    prop "encode/decode is identity" $ \(n :: Int16) ->
      pgDecode (pgEncode n) === Right n

  describe "Int32 round-trip property" $
    prop "encode/decode is identity" $ \(n :: Int32) ->
      pgDecode (pgEncode n) === Right n

  describe "Int64 round-trip property" $
    prop "encode/decode is identity" $ \(n :: Int64) ->
      pgDecode (pgEncode n) === Right n

  describe "Float round-trip property" $
    prop "encode/decode is identity (non-NaN)" $ \(n :: Float) ->
      not (isNaN n) ==> pgDecode (pgEncode n) === Right n

  describe "Double round-trip property" $
    prop "encode/decode is identity (non-NaN)" $ \(n :: Double) ->
      not (isNaN n) ==> pgDecode (pgEncode n) === Right n

  describe "Text round-trip property" $
    prop "encode/decode is identity" $ \(s :: String) ->
      let t = T.pack s
       in pgDecode (pgEncode t) === Right t

  describe "ByteString round-trip property" $
    prop "encode/decode is identity" $ \(bs :: [Word8]) ->
      let b = BS.pack bs
       in pgDecode (pgEncode b) === Right b

  describe "Day round-trip property" $
    prop "encode/decode is identity" $ forAll genDay $ \d ->
      pgDecode (pgEncode d) === Right d

  describe "TimeOfDay round-trip property" $
    prop "encode/decode is identity" $ forAll genTimeOfDay $ \tod ->
      pgDecode (pgEncode tod) === Right tod

  describe "UTCTime round-trip property" $
    prop "encode/decode is identity (microsecond precision)" $ forAll genUTCTime $ \t ->
      pgDecode (pgEncode t) === Right t

  describe "LocalTime round-trip property" $
    prop "encode/decode is identity (microsecond precision)" $ forAll genLocalTime $ \lt ->
      pgDecode (pgEncode lt) === Right lt

  describe "PgInterval round-trip property" $
    prop "encode/decode is identity" $ forAll genInterval $ \iv ->
      pgDecode (pgEncode iv) === Right iv

  describe "Scientific round-trip property" $
    prop "encode/decode is identity (reasonable range)" $ forAll genScientific $ \s ->
      pgDecode (pgEncode s) === Right s

  describe "Int32 array round-trip property" $
    prop "encode/decode is identity" $ \(xs :: [Int32]) ->
      let v = V.fromList xs
       in pgDecodeArray (pgEncodeArray oidInt4 v) === Right v

  describe "Text array round-trip property" $
    prop "encode/decode is identity" $ \(xs :: [String]) ->
      let v = V.fromList (map T.pack xs)
       in pgDecodeArray (pgEncodeArray oidText v) === Right v

  describe "Bool array round-trip property" $
    prop "encode/decode is identity" $ \(xs :: [Bool]) ->
      let v = V.fromList xs
       in pgDecodeArray (pgEncodeArray oidBool v) === Right v

  describe "Int64 array round-trip property" $
    prop "encode/decode is identity" $ \(xs :: [Int64]) ->
      let v = V.fromList xs
       in pgDecodeArray (pgEncodeArray oidInt8 v) === Right v

  describe "Double array round-trip property" $
    prop "encode/decode is identity (non-NaN)" $ \(xs :: [Double]) ->
      not (any isNaN xs)
        ==> let v = V.fromList xs
             in pgDecodeArray (pgEncodeArray oidFloat8 v) === Right v

  describe "UUID round-trip property" $
    prop "encode/decode is identity" $ forAll genUUID $ \u ->
      pgDecode (pgEncode u) === Right u

  describe "PgRange Int32 round-trip property" $
    prop "encode/decode is identity" $ forAll genInt32Range $ \r ->
      pgDecodeRange pgDecode (pgEncodeRange pgEncode r) === Right r

  describe "JSON Value round-trip property" $
    prop "encode/decode is identity" $ forAll genJsonValue $ \v ->
      pgDecode (pgEncode v) === Right v

  describe "encode output size" $ do
    prop "Bool encodes to 1 byte" $ \(b :: Bool) ->
      BS.length (pgEncode b) === 1

    prop "Int16 encodes to 2 bytes" $ \(n :: Int16) ->
      BS.length (pgEncode n) === 2

    prop "Int32 encodes to 4 bytes" $ \(n :: Int32) ->
      BS.length (pgEncode n) === 4

    prop "Int64 encodes to 8 bytes" $ \(n :: Int64) ->
      BS.length (pgEncode n) === 8

    prop "Day encodes to 4 bytes" $ forAll genDay $ \d ->
      BS.length (pgEncode d) === 4

    prop "TimeOfDay encodes to 8 bytes" $ forAll genTimeOfDay $ \tod ->
      BS.length (pgEncode tod) === 8

    prop "PgInterval encodes to 16 bytes" $ forAll genInterval $ \iv ->
      BS.length (pgEncode iv) === 16

    prop "UUID encodes to 16 bytes" $ forAll genUUID $ \u ->
      BS.length (pgEncode u) === 16

    prop "Empty range encodes to 1 byte" $
      BS.length (pgEncodeRange (pgEncode @Int32) EmptyRange) === 1

-- Generators ----------------------------------------------------------------

genDay :: Gen Day
genDay = do
  y <- choose (1900, 2100)
  m <- choose (1, 12)
  d <- choose (1, 28)
  pure (fromGregorian y m d)

genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = do
  h <- choose (0, 23)
  m <- choose (0, 59)
  micros <- choose (0, 59999999 :: Int)
  pure (TimeOfDay h m (fromIntegral micros / 1000000))

genUTCTime :: Gen UTCTime
genUTCTime = do
  d <- genDay
  micros <- choose (0, 86399999999 :: Int64)
  let picos = fromIntegral micros * 1000000
  pure (UTCTime d (picosecondsToDiffTime picos))

genLocalTime :: Gen LocalTime
genLocalTime = LocalTime <$> genDay <*> genTimeOfDay

genInterval :: Gen PgInterval
genInterval =
  PgInterval
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

genScientific :: Gen Scientific
genScientific = do
  coeff <- choose (-999999999, 999999999 :: Integer)
  expo <- choose (-8, 8 :: Int)
  pure (scientific coeff expo)

genUUID :: Gen UUID
genUUID = do
  w1 <- arbitrary @Word32
  w2 <- arbitrary @Word32
  w3 <- arbitrary @Word32
  w4 <- arbitrary @Word32
  pure (UUID.fromWords w1 w2 w3 w4)

genRangeBound :: Gen a -> Gen (Maybe (RangeBound a))
genRangeBound gen =
  oneof
    [ pure Nothing
    , Just . Inclusive <$> gen
    , Just . Exclusive <$> gen
    ]

genInt32Range :: Gen (PgRange Int32)
genInt32Range =
  frequency
    [ (1, pure EmptyRange)
    , (9, PgRange <$> genRangeBound arbitrary <*> genRangeBound arbitrary)
    ]

genJsonValue :: Gen Value
genJsonValue = sized go
  where
    go 0 = genLeaf
    go n =
      oneof
        [ genLeaf
        , Array . V.fromList <$> resize (n `div` 4) (listOf (go (n `div` 4)))
        , do
            pairs <- resize (n `div` 4) (listOf genPair)
            pure (object pairs)
        ]
      where
        genPair = do
          k <- Key.fromText . T.pack <$> listOf (elements ['a' .. 'z'])
          v <- go (n `div` 4)
          pure (k .= v)

    genLeaf =
      oneof
        [ pure Null
        , Bool <$> arbitrary
        , Number . fromIntegral <$> choose (-1000, 1000 :: Int)
        , String . T.pack <$> listOf (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ " "))
        ]

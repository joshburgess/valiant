module Valiant.Binary.PropertyHedgehogSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.Int (Int32)
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
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Valiant.Binary.Array (pgDecodeArray, pgEncodeArray)
import Valiant.Binary.Decode ()
import Valiant.Binary.Encode ()
import Valiant.Binary.Interval (PgInterval (..))
import Valiant.Binary.JSON ()
import Valiant.Binary.Range (PgRange (..), RangeBound (..), pgDecodeRange, pgEncodeRange)
import Valiant.Binary.Scientific ()
import Valiant.Binary.UUID ()
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import PgWire.Protocol.Oid
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  -- ── Scalar round-trips ──────────────────────────────────────────
  describe "Bool round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      b <- forAll Gen.bool
      pgDecode (pgEncode b) === Right b

  describe "Int16 round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      n <- forAll (Gen.int16 Range.linearBounded)
      pgDecode (pgEncode n) === Right n

  describe "Int32 round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      n <- forAll (Gen.int32 Range.linearBounded)
      pgDecode (pgEncode n) === Right n

  describe "Int64 round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      n <- forAll (Gen.int64 Range.linearBounded)
      pgDecode (pgEncode n) === Right n

  describe "Float round-trip (hedgehog)" $
    it "encode/decode is identity (non-NaN)" $ hedgehog $ do
      n <- forAll (Gen.filter (not . isNaN) (Gen.float (Range.linearFrac (-1e6) 1e6)))
      pgDecode (pgEncode n) === Right n

  describe "Double round-trip (hedgehog)" $
    it "encode/decode is identity (non-NaN)" $ hedgehog $ do
      n <- forAll (Gen.filter (not . isNaN) (Gen.double (Range.linearFrac (-1e15) 1e15)))
      pgDecode (pgEncode n) === Right n

  describe "Text round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      t <- forAll (Gen.text (Range.linear 0 200) Gen.unicode)
      pgDecode (pgEncode t) === Right t

  describe "ByteString round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      bs <- forAll (Gen.bytes (Range.linear 0 200))
      pgDecode (pgEncode bs) === Right bs

  -- ── Time types ──────────────────────────────────────────────────
  describe "Day round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      d <- forAll genDay
      pgDecode (pgEncode d) === Right d

  describe "TimeOfDay round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      tod <- forAll genTimeOfDay
      pgDecode (pgEncode tod) === Right tod

  describe "UTCTime round-trip (hedgehog)" $
    it "encode/decode is identity (microsecond precision)" $ hedgehog $ do
      t <- forAll genUTCTime
      pgDecode (pgEncode t) === Right t

  describe "LocalTime round-trip (hedgehog)" $
    it "encode/decode is identity (microsecond precision)" $ hedgehog $ do
      lt <- forAll genLocalTime
      pgDecode (pgEncode lt) === Right lt

  describe "PgInterval round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      iv <- forAll genInterval
      pgDecode (pgEncode iv) === Right iv

  -- ── Numeric types ───────────────────────────────────────────────
  describe "Scientific round-trip (hedgehog)" $
    it "encode/decode is identity (reasonable range)" $ hedgehog $ do
      s <- forAll genScientific
      pgDecode (pgEncode s) === Right s

  -- ── Array types ─────────────────────────────────────────────────
  describe "Int32 array round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int32 Range.linearBounded))
      let v = V.fromList xs
      pgDecodeArray (pgEncodeArray oidInt4 v) === Right v

  describe "Text array round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      xs <- forAll (Gen.list (Range.linear 0 50) (Gen.text (Range.linear 0 20) Gen.alphaNum))
      let v = V.fromList xs
      pgDecodeArray (pgEncodeArray oidText v) === Right v

  describe "Bool array round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      xs <- forAll (Gen.list (Range.linear 0 50) Gen.bool)
      let v = V.fromList xs
      pgDecodeArray (pgEncodeArray oidBool v) === Right v

  describe "Int64 array round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int64 Range.linearBounded))
      let v = V.fromList xs
      pgDecodeArray (pgEncodeArray oidInt8 v) === Right v

  describe "Double array round-trip (hedgehog)" $
    it "encode/decode is identity (non-NaN)" $ hedgehog $ do
      xs <- forAll (Gen.list (Range.linear 0 50)
        (Gen.filter (not . isNaN) (Gen.double (Range.linearFrac (-1e15) 1e15))))
      let v = V.fromList xs
      pgDecodeArray (pgEncodeArray oidFloat8 v) === Right v

  -- ── UUID ────────────────────────────────────────────────────────
  describe "UUID round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      u <- forAll genUUID
      pgDecode (pgEncode u) === Right u

  -- ── Range ───────────────────────────────────────────────────────
  describe "PgRange Int32 round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      r <- forAll genInt32Range
      pgDecodeRange pgDecode (pgEncodeRange pgEncode r) === Right r

  -- ── JSON ────────────────────────────────────────────────────────
  describe "JSON Value round-trip (hedgehog)" $
    it "encode/decode is identity" $ hedgehog $ do
      v <- forAll genJsonValue
      pgDecode (pgEncode v) === Right v

  -- ── Encode sizes ────────────────────────────────────────────────
  describe "Encode output size (hedgehog)" $ do
    it "Bool encodes to 1 byte" $ hedgehog $ do
      b <- forAll Gen.bool
      BS.length (pgEncode b) === 1

    it "Int16 encodes to 2 bytes" $ hedgehog $ do
      n <- forAll (Gen.int16 Range.linearBounded)
      BS.length (pgEncode n) === 2

    it "Int32 encodes to 4 bytes" $ hedgehog $ do
      n <- forAll (Gen.int32 Range.linearBounded)
      BS.length (pgEncode n) === 4

    it "Int64 encodes to 8 bytes" $ hedgehog $ do
      n <- forAll (Gen.int64 Range.linearBounded)
      BS.length (pgEncode n) === 8

    it "Day encodes to 4 bytes" $ hedgehog $ do
      d <- forAll genDay
      BS.length (pgEncode d) === 4

    it "TimeOfDay encodes to 8 bytes" $ hedgehog $ do
      tod <- forAll genTimeOfDay
      BS.length (pgEncode tod) === 8

    it "PgInterval encodes to 16 bytes" $ hedgehog $ do
      iv <- forAll genInterval
      BS.length (pgEncode iv) === 16

    it "UUID encodes to 16 bytes" $ hedgehog $ do
      u <- forAll genUUID
      BS.length (pgEncode u) === 16

    it "Empty range encodes to 1 byte" $
      BS.length (pgEncodeRange (pgEncode @Int32) EmptyRange) `shouldBe` 1

-- ── Generators ──────────────────────────────────────────────────────

genDay :: Gen Day
genDay = do
  y <- Gen.integral (Range.linear 1900 2100 :: Range Integer)
  m <- Gen.int (Range.linear 1 12)
  d <- Gen.int (Range.linear 1 28)
  pure (fromGregorian (fromIntegral y) m d)

genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = do
  h <- Gen.int (Range.linear 0 23)
  m <- Gen.int (Range.linear 0 59)
  micros <- Gen.int (Range.linear 0 59999999)
  pure (TimeOfDay h m (fromIntegral micros / 1000000))

genUTCTime :: Gen UTCTime
genUTCTime = do
  d <- genDay
  micros <- Gen.int64 (Range.linear 0 86399999999)
  let picos = fromIntegral micros * 1000000
  pure (UTCTime d (picosecondsToDiffTime picos))

genLocalTime :: Gen LocalTime
genLocalTime = LocalTime <$> genDay <*> genTimeOfDay

genInterval :: Gen PgInterval
genInterval =
  PgInterval
    <$> Gen.int64 Range.linearBounded
    <*> Gen.int32 Range.linearBounded
    <*> Gen.int32 Range.linearBounded

genScientific :: Gen Scientific
genScientific = do
  coeff <- Gen.integral (Range.linear (-999999999) 999999999)
  expo <- Gen.int (Range.linear (-8) 8)
  pure (scientific coeff expo)

genUUID :: Gen UUID
genUUID = do
  w1 <- Gen.word32 Range.linearBounded
  w2 <- Gen.word32 Range.linearBounded
  w3 <- Gen.word32 Range.linearBounded
  w4 <- Gen.word32 Range.linearBounded
  pure (UUID.fromWords w1 w2 w3 w4)

genRangeBound :: Gen a -> Gen (Maybe (RangeBound a))
genRangeBound gen =
  Gen.choice
    [ pure Nothing
    , Just . Inclusive <$> gen
    , Just . Exclusive <$> gen
    ]

genInt32Range :: Gen (PgRange Int32)
genInt32Range =
  Gen.frequency
    [ (1, pure EmptyRange)
    , (9, PgRange <$> genRangeBound (Gen.int32 Range.linearBounded) <*> genRangeBound (Gen.int32 Range.linearBounded))
    ]

genJsonValue :: Gen Value
genJsonValue = Gen.recursive Gen.choice
  -- base cases
  [ pure Null
  , Bool <$> Gen.bool
  , Number . fromIntegral <$> Gen.int (Range.linear (-1000) 1000)
  , String . T.pack <$> Gen.list (Range.linear 0 20) (Gen.element (['a'..'z'] ++ ['0'..'9'] ++ " "))
  ]
  -- recursive cases
  [ Array . V.fromList <$> Gen.list (Range.linear 0 4) genJsonValue
  , do
      pairs <- Gen.list (Range.linear 0 4) genJsonPair
      pure (object pairs)
  ]
  where
    genJsonPair = do
      k <- Key.fromText . T.pack <$> Gen.list (Range.linear 1 10) (Gen.element ['a'..'z'])
      v <- genJsonValue
      pure (k .= v)

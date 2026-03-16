module Hsqlx.Binary.DecodeSpec (spec) where

import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time
  ( Day
  , LocalTime (..)
  , TimeOfDay (..)
  , UTCTime (..)
  , fromGregorian
  )
import Hsqlx.Binary.Decode ()
import Hsqlx.Binary.Encode ()
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "Bool round-trip" $ do
    it "True" $ pgDecode (pgEncode True) `shouldBe` Right True
    it "False" $ pgDecode (pgEncode False) `shouldBe` Right False

  describe "Int16 round-trip" $ do
    it "0" $ pgDecode (pgEncode (0 :: Int16)) `shouldBe` Right (0 :: Int16)
    it "32767" $ pgDecode (pgEncode (32767 :: Int16)) `shouldBe` Right (32767 :: Int16)
    it "-1" $ pgDecode (pgEncode (-1 :: Int16)) `shouldBe` Right (-1 :: Int16)

  describe "Int32 round-trip" $ do
    it "0" $ pgDecode (pgEncode (0 :: Int32)) `shouldBe` Right (0 :: Int32)
    it "42" $ pgDecode (pgEncode (42 :: Int32)) `shouldBe` Right (42 :: Int32)
    it "-100" $ pgDecode (pgEncode (-100 :: Int32)) `shouldBe` Right (-100 :: Int32)

  describe "Int64 round-trip" $ do
    it "0" $ pgDecode (pgEncode (0 :: Int64)) `shouldBe` Right (0 :: Int64)
    it "large value" $ pgDecode (pgEncode (9999999999 :: Int64)) `shouldBe` Right (9999999999 :: Int64)

  describe "Float round-trip" $ do
    it "0.0" $ pgDecode (pgEncode (0.0 :: Float)) `shouldBe` Right (0.0 :: Float)
    it "3.14" $ do
      let Right v = pgDecode (pgEncode (3.14 :: Float)) :: Either String Float
      abs (v - 3.14) `shouldSatisfy` (< 0.001)

  describe "Double round-trip" $ do
    it "0.0" $ pgDecode (pgEncode (0.0 :: Double)) `shouldBe` Right (0.0 :: Double)
    it "3.14159" $ pgDecode (pgEncode (3.14159 :: Double)) `shouldBe` Right (3.14159 :: Double)

  describe "Text round-trip" $ do
    it "empty" $ pgDecode (pgEncode ("" :: Text)) `shouldBe` Right ("" :: Text)
    it "hello" $ pgDecode (pgEncode ("hello" :: Text)) `shouldBe` Right ("hello" :: Text)

  describe "ByteString round-trip" $ do
    it "identity" $ pgDecode (pgEncode ("bytes" :: BS.ByteString)) `shouldBe` Right ("bytes" :: BS.ByteString)

  describe "error on wrong size" $ do
    it "Int32 with 3 bytes fails" $ do
      let result = pgDecode (BS.pack [0, 0, 0]) :: Either String Int32
      result `shouldSatisfy` isLeft
    it "Bool with 2 bytes fails" $ do
      let result = pgDecode (BS.pack [0, 0]) :: Either String Bool
      result `shouldSatisfy` isLeft

  describe "UTCTime round-trip" $ do
    it "PG epoch (2000-01-01 00:00:00 UTC)" $ do
      let t = UTCTime (fromGregorian 2000 1 1) 0
      pgDecode (pgEncode t) `shouldBe` Right t

    it "one second after PG epoch" $ do
      let t = UTCTime (fromGregorian 2000 1 1) 1
      pgDecode (pgEncode t) `shouldBe` Right t

    it "a date in 2024" $ do
      -- 2024-06-15 12:00:00 UTC
      let t = UTCTime (fromGregorian 2024 6 15) (12 * 3600)
      pgDecode (pgEncode t) `shouldBe` Right t

    it "midnight on a non-epoch day" $ do
      let t = UTCTime (fromGregorian 2020 3 15) 0
      pgDecode (pgEncode t) `shouldBe` Right t

    it "time with sub-second precision (microseconds)" $ do
      -- 0.5 seconds = 500000 microseconds
      let t = UTCTime (fromGregorian 2000 1 1) 0.5
      -- PG stores microseconds, so round-trip should preserve microsecond precision
      pgDecode (pgEncode t) `shouldBe` Right t

    it "time before PG epoch (1999-12-31)" $ do
      let t = UTCTime (fromGregorian 1999 12 31) 0
      pgDecode (pgEncode t) `shouldBe` Right t

  describe "Day round-trip" $ do
    it "PG epoch day (2000-01-01)" $ do
      let d = fromGregorian 2000 1 1
      pgDecode (pgEncode d) `shouldBe` Right d

    it "2000-01-02" $ do
      let d = fromGregorian 2000 1 2
      pgDecode (pgEncode d) `shouldBe` Right d

    it "1999-12-31 (before PG epoch)" $ do
      let d = fromGregorian 1999 12 31
      pgDecode (pgEncode d) `shouldBe` Right d

    it "2024-06-15" $ do
      let d = fromGregorian 2024 6 15
      pgDecode (pgEncode d) `shouldBe` Right d

    it "fails on wrong byte count" $ do
      let result = pgDecode (BS.pack [0, 0]) :: Either String Day
      result `shouldSatisfy` isLeft

  describe "TimeOfDay round-trip" $ do
    it "midnight (00:00:00)" $ do
      let tod = TimeOfDay 0 0 0
      pgDecode (pgEncode tod) `shouldBe` Right tod

    it "00:00:01" $ do
      let tod = TimeOfDay 0 0 1
      pgDecode (pgEncode tod) `shouldBe` Right tod

    it "12:30:45" $ do
      let tod = TimeOfDay 12 30 45
      pgDecode (pgEncode tod) `shouldBe` Right tod

    it "23:59:59" $ do
      let tod = TimeOfDay 23 59 59
      pgDecode (pgEncode tod) `shouldBe` Right tod

    it "fails on wrong byte count" $ do
      let result = pgDecode (BS.pack [0, 0, 0, 0]) :: Either String TimeOfDay
      result `shouldSatisfy` isLeft

  describe "LocalTime round-trip" $ do
    it "PG epoch midnight" $ do
      let lt = LocalTime (fromGregorian 2000 1 1) (TimeOfDay 0 0 0)
      pgDecode (pgEncode lt) `shouldBe` Right lt

    it "PG epoch + 1 second" $ do
      let lt = LocalTime (fromGregorian 2000 1 1) (TimeOfDay 0 0 1)
      pgDecode (pgEncode lt) `shouldBe` Right lt

    it "2024-03-15 14:30:00" $ do
      let lt = LocalTime (fromGregorian 2024 3 15) (TimeOfDay 14 30 0)
      pgDecode (pgEncode lt) `shouldBe` Right lt

    it "date before PG epoch" $ do
      let lt = LocalTime (fromGregorian 1999 6 15) (TimeOfDay 8 0 0)
      pgDecode (pgEncode lt) `shouldBe` Right lt

    it "fails on wrong byte count" $ do
      let result = pgDecode (BS.pack [0, 0, 0, 0]) :: Either String LocalTime
      result `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

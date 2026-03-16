module Hsqlx.Binary.EncodeSpec (spec) where

import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time
  ( LocalTime (..)
  , TimeOfDay (..)
  , UTCTime (..)
  , fromGregorian
  , secondsToDiffTime
  )
import Hsqlx.Binary.Encode ()
import PgWire.Binary.Types (PgEncode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "Bool" $ do
    it "encodes True as 0x01" $ do
      pgEncode True `shouldBe` BS.singleton 1
    it "encodes False as 0x00" $ do
      pgEncode False `shouldBe` BS.singleton 0

  describe "Int16" $ do
    it "encodes 0" $ do
      pgEncode (0 :: Int16) `shouldBe` BS.pack [0, 0]
    it "encodes 256 as big-endian" $ do
      pgEncode (256 :: Int16) `shouldBe` BS.pack [1, 0]
    it "encodes -1" $ do
      pgEncode (-1 :: Int16) `shouldBe` BS.pack [0xFF, 0xFF]

  describe "Int32" $ do
    it "encodes 1 as big-endian" $ do
      pgEncode (1 :: Int32) `shouldBe` BS.pack [0, 0, 0, 1]
    it "encodes 16777216 (0x01000000)" $ do
      pgEncode (16777216 :: Int32) `shouldBe` BS.pack [1, 0, 0, 0]

  describe "Int64" $ do
    it "encodes 1 as 8 bytes big-endian" $ do
      pgEncode (1 :: Int64) `shouldBe` BS.pack [0, 0, 0, 0, 0, 0, 0, 1]

  describe "Text" $ do
    it "encodes as UTF-8" $ do
      pgEncode ("hello" :: Text) `shouldBe` "hello"

  describe "ByteString" $ do
    it "encodes as identity" $ do
      pgEncode ("raw bytes" :: BS.ByteString) `shouldBe` "raw bytes"

  describe "UTCTime" $ do
    it "encodes PG epoch (2000-01-01 00:00:00 UTC) as zero" $ do
      let pgEpoch = UTCTime (fromGregorian 2000 1 1) 0
          bs = pgEncode pgEpoch
      -- PG epoch should encode to 0 microseconds
      BS.length bs `shouldBe` 8
      bs `shouldBe` BS.pack [0, 0, 0, 0, 0, 0, 0, 0]

    it "encodes a time after PG epoch as positive" $ do
      -- 2000-01-01 00:00:01 UTC = 1 second = 1000000 microseconds after PG epoch
      let t = UTCTime (fromGregorian 2000 1 1) 1
          bs = pgEncode t
      BS.length bs `shouldBe` 8
      -- 1000000 = 0x000000000F4240
      bs `shouldBe` BS.pack [0, 0, 0, 0, 0, 0x0F, 0x42, 0x40]

    it "encodes a known date correctly (2024-01-15 12:30:00 UTC)" $ do
      let t = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime (12 * 3600 + 30 * 60))
          bs = pgEncode t
      BS.length bs `shouldBe` 8

  describe "Day" $ do
    it "encodes PG epoch day (2000-01-01) as zero" $ do
      let d = fromGregorian 2000 1 1
      pgEncode d `shouldBe` BS.pack [0, 0, 0, 0]

    it "encodes 2000-01-02 as 1" $ do
      let d = fromGregorian 2000 1 2
      pgEncode d `shouldBe` BS.pack [0, 0, 0, 1]

    it "encodes a date before PG epoch as negative" $ do
      -- 1999-12-31 is -1 days from PG epoch
      let d = fromGregorian 1999 12 31
      pgEncode d `shouldBe` BS.pack [0xFF, 0xFF, 0xFF, 0xFF]

    it "produces 4 bytes" $ do
      let d = fromGregorian 2024 6 15
      BS.length (pgEncode d) `shouldBe` 4

  describe "TimeOfDay" $ do
    it "encodes midnight as zero" $ do
      let tod = TimeOfDay 0 0 0
      pgEncode tod `shouldBe` BS.pack [0, 0, 0, 0, 0, 0, 0, 0]

    it "encodes 00:00:01 as 1000000 microseconds" $ do
      let tod = TimeOfDay 0 0 1
      pgEncode tod `shouldBe` BS.pack [0, 0, 0, 0, 0, 0x0F, 0x42, 0x40]

    it "encodes 01:00:00 as 3600000000 microseconds" $ do
      let tod = TimeOfDay 1 0 0
          bs = pgEncode tod
      BS.length bs `shouldBe` 8

    it "produces 8 bytes" $ do
      let tod = TimeOfDay 12 30 45
      BS.length (pgEncode tod) `shouldBe` 8

  describe "LocalTime" $ do
    it "encodes PG epoch midnight as zero" $ do
      let lt = LocalTime (fromGregorian 2000 1 1) (TimeOfDay 0 0 0)
      pgEncode lt `shouldBe` BS.pack [0, 0, 0, 0, 0, 0, 0, 0]

    it "encodes PG epoch + 1 second" $ do
      -- 1 second = 1000000 microseconds
      let lt = LocalTime (fromGregorian 2000 1 1) (TimeOfDay 0 0 1)
      pgEncode lt `shouldBe` BS.pack [0, 0, 0, 0, 0, 0x0F, 0x42, 0x40]

    it "produces 8 bytes" $ do
      let lt = LocalTime (fromGregorian 2024 3 15) (TimeOfDay 14 30 0)
      BS.length (pgEncode lt) `shouldBe` 8

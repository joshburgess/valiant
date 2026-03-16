module Hsqlx.Binary.IntervalSpec (spec) where

import Data.ByteString qualified as BS
import Hsqlx.Binary.Interval (PgInterval (..))
import Hsqlx.Binary.Types (PgDecode (..), PgEncode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "interval round-trip" $ do
    it "zero interval" $ do
      let iv = PgInterval 0 0 0
      pgDecode (pgEncode iv) `shouldBe` Right iv

    it "1 second" $ do
      let iv = PgInterval 1000000 0 0
      pgDecode (pgEncode iv) `shouldBe` Right iv

    it "1 day" $ do
      let iv = PgInterval 0 1 0
      pgDecode (pgEncode iv) `shouldBe` Right iv

    it "1 month" $ do
      let iv = PgInterval 0 0 1
      pgDecode (pgEncode iv) `shouldBe` Right iv

    it "complex interval (1 year, 2 months, 3 days, 4 hours)" $ do
      let iv = PgInterval (4 * 3600 * 1000000) 3 14 -- 14 months = 1y2m
      pgDecode (pgEncode iv) `shouldBe` Right iv

    it "negative microseconds" $ do
      let iv = PgInterval (-500000) 0 0
      pgDecode (pgEncode iv) `shouldBe` Right iv

    it "negative days" $ do
      let iv = PgInterval 0 (-7) 0
      pgDecode (pgEncode iv) `shouldBe` Right iv

    it "negative months" $ do
      let iv = PgInterval 0 0 (-3)
      pgDecode (pgEncode iv) `shouldBe` Right iv

  describe "interval binary format" $ do
    it "produces exactly 16 bytes" $ do
      let iv = PgInterval 12345 67 8
      BS.length (pgEncode iv) `shouldBe` 16

    it "fails on wrong byte count" $ do
      let result = pgDecode (BS.pack [0, 0, 0, 0]) :: Either String PgInterval
      result `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

module Valiant.Binary.ArraySpec (spec) where

import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Valiant.Binary.Array (pgDecodeArray, pgEncodeArray)
import Valiant.Binary.Decode ()
import Valiant.Binary.Encode ()
import PgWire.Protocol.Oid
import Test.Hspec

spec :: Spec
spec = do
  describe "Int32 array round-trip" $ do
    it "empty array" $ do
      let arr = V.empty :: Vector Int32
          encoded = pgEncodeArray oidInt4 arr
      pgDecodeArray encoded `shouldBe` Right arr

    it "single element" $ do
      let arr = V.singleton (42 :: Int32)
          encoded = pgEncodeArray oidInt4 arr
      pgDecodeArray encoded `shouldBe` Right arr

    it "multiple elements" $ do
      let arr = V.fromList [1, 2, 3, 4, 5] :: Vector Int32
          encoded = pgEncodeArray oidInt4 arr
      pgDecodeArray encoded `shouldBe` Right arr

    it "negative values" $ do
      let arr = V.fromList [-100, 0, 100] :: Vector Int32
          encoded = pgEncodeArray oidInt4 arr
      pgDecodeArray encoded `shouldBe` Right arr

  describe "Int64 array round-trip" $ do
    it "multiple elements" $ do
      let arr = V.fromList [1000000, 2000000, 3000000] :: Vector Int64
          encoded = pgEncodeArray oidInt8 arr
      pgDecodeArray encoded `shouldBe` Right arr

  describe "Text array round-trip" $ do
    it "multiple strings" $ do
      let arr = V.fromList ["hello", "world", "foo"] :: Vector Text
          encoded = pgEncodeArray oidText arr
      pgDecodeArray encoded `shouldBe` Right arr

    it "empty strings" $ do
      let arr = V.fromList ["", "", "x"] :: Vector Text
          encoded = pgEncodeArray oidText arr
      pgDecodeArray encoded `shouldBe` Right arr

  describe "Bool array round-trip" $ do
    it "mixed booleans" $ do
      let arr = V.fromList [True, False, True, False] :: Vector Bool
          encoded = pgEncodeArray oidBool arr
      pgDecodeArray encoded `shouldBe` Right arr

  describe "decode errors" $ do
    it "fails on truncated input" $ do
      let result = pgDecodeArray (BS.pack [0, 0, 0, 1]) :: Either String (Vector Int32)
      result `shouldSatisfy` isLeft

    it "fails on empty input" $ do
      let result = pgDecodeArray BS.empty :: Either String (Vector Int32)
      result `shouldSatisfy` isLeft

  describe "binary format" $ do
    it "produces correct header for empty 1-D array" $ do
      let encoded = pgEncodeArray oidInt4 (V.empty :: Vector Int32)
      -- ndim=1, no null, oid=23, size=0, lbound=1
      BS.length encoded `shouldBe` 20
      -- First 4 bytes: ndim = 1
      BS.take 4 encoded `shouldBe` BS.pack [0, 0, 0, 1]

    it "element data follows header" $ do
      let encoded = pgEncodeArray oidInt4 (V.singleton (42 :: Int32))
      -- 20 bytes header + 4 bytes element length + 4 bytes element data = 28
      BS.length encoded `shouldBe` 28

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

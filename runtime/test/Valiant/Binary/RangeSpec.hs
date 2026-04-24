module Valiant.Binary.RangeSpec (spec) where

import Data.ByteString qualified as BS
import Data.Int (Int32)
import Valiant.Binary.Decode ()
import Valiant.Binary.Encode ()
import Valiant.Binary.Range
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "range round-trip" $ do
    it "empty range" $ do
      let r = EmptyRange :: PgRange Int32
      pgDecodeRange pgDecode (pgEncodeRange pgEncode r) `shouldBe` Right r

    it "inclusive-inclusive [1, 10]" $ do
      let r = PgRange (Just (Inclusive (1 :: Int32))) (Just (Inclusive 10))
      pgDecodeRange pgDecode (pgEncodeRange pgEncode r) `shouldBe` Right r

    it "exclusive-exclusive (1, 10)" $ do
      let r = PgRange (Just (Exclusive (1 :: Int32))) (Just (Exclusive 10))
      pgDecodeRange pgDecode (pgEncodeRange pgEncode r) `shouldBe` Right r

    it "inclusive-exclusive [1, 10)" $ do
      let r = PgRange (Just (Inclusive (1 :: Int32))) (Just (Exclusive 10))
      pgDecodeRange pgDecode (pgEncodeRange pgEncode r) `shouldBe` Right r

    it "unbounded lower (,10]" $ do
      let r = PgRange Nothing (Just (Inclusive (10 :: Int32)))
      pgDecodeRange pgDecode (pgEncodeRange pgEncode r) `shouldBe` Right r

    it "unbounded upper [1,)" $ do
      let r = PgRange (Just (Inclusive (1 :: Int32))) Nothing
      pgDecodeRange pgDecode (pgEncodeRange pgEncode r) `shouldBe` Right r

    it "fully unbounded (,)" $ do
      let r = PgRange Nothing Nothing :: PgRange Int32
      pgDecodeRange pgDecode (pgEncodeRange pgEncode r) `shouldBe` Right r

    it "negative values [-100, -1]" $ do
      let r = PgRange (Just (Inclusive (-100 :: Int32))) (Just (Inclusive (-1)))
      pgDecodeRange pgDecode (pgEncodeRange pgEncode r) `shouldBe` Right r

  describe "range decode errors" $ do
    it "fails on empty input" $ do
      let result = pgDecodeRange (pgDecode @Int32) BS.empty
      result `shouldSatisfy` isLeft

  describe "range binary format" $ do
    it "empty range is a single byte" $ do
      let encoded = pgEncodeRange (pgEncode @Int32) EmptyRange
      BS.length encoded `shouldBe` 1
      BS.index encoded 0 `shouldBe` 0x01

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

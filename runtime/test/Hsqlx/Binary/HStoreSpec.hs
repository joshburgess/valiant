module Hsqlx.Binary.HStoreSpec (spec) where

import Data.Map.Strict qualified as Map
import Hsqlx.Binary.HStore (PgHStore (..), hstoreFromList, hstoreToList)
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "hstore round-trip" $ do
    it "empty map" $ do
      let v = PgHStore Map.empty
      pgDecode (pgEncode v) `shouldBe` Right v

    it "single pair" $ do
      let v = hstoreFromList [("key", "value")]
      pgDecode (pgEncode v) `shouldBe` Right v

    it "multiple pairs" $ do
      let v = hstoreFromList [("a", "1"), ("b", "2"), ("c", "3")]
      pgDecode (pgEncode v) `shouldBe` Right v

    it "preserves NULL values" $ do
      let v = PgHStore (Map.fromList [("present", Just "yes"), ("absent", Nothing)])
      pgDecode (pgEncode v) `shouldBe` Right v

    it "empty key" $ do
      let v = hstoreFromList [("", "empty-key")]
      pgDecode (pgEncode v) `shouldBe` Right v

    it "empty value" $ do
      let v = hstoreFromList [("key", "")]
      pgDecode (pgEncode v) `shouldBe` Right v

    it "unicode" $ do
      let v = hstoreFromList [("名前", "太郎"), ("stadt", "München")]
      pgDecode (pgEncode v) `shouldBe` Right v

  describe "hstoreToList" $ do
    it "filters out NULL values" $ do
      let v = PgHStore (Map.fromList [("a", Just "1"), ("b", Nothing), ("c", Just "3")])
      hstoreToList v `shouldBe` [("a", "1"), ("c", "3")]

    it "empty on all NULLs" $ do
      let v = PgHStore (Map.fromList [("x", Nothing)])
      hstoreToList v `shouldBe` []

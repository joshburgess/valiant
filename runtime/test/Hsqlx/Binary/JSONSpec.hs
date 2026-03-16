module Hsqlx.Binary.JSONSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.ByteString qualified as BS
import Hsqlx.Binary.JSON ()
import Hsqlx.Binary.Types (PgDecode (..), PgEncode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "JSON round-trip (json format)" $ do
    it "null" $ do
      pgDecode (pgEncode Null) `shouldBe` Right Null

    it "boolean true" $ do
      pgDecode (pgEncode (Bool True)) `shouldBe` Right (Bool True)

    it "number" $ do
      pgDecode (pgEncode (Number 42)) `shouldBe` Right (Number 42)

    it "string" $ do
      pgDecode (pgEncode (String "hello")) `shouldBe` Right (String "hello")

    it "empty object" $ do
      pgDecode (pgEncode (object [])) `shouldBe` Right (object [])

    it "object with fields" $ do
      let v = object ["name" .= String "Alice", "age" .= Number 30]
      pgDecode (pgEncode v) `shouldBe` Right v

    it "array" $ do
      let v = Array mempty
      pgDecode (pgEncode v) `shouldBe` Right v

    it "nested structure" $ do
      let v = object ["users" .= [object ["id" .= Number 1, "name" .= String "Bob"]]]
      pgDecode (pgEncode v) `shouldBe` Right v

  describe "JSONB decode (version byte prefix)" $ do
    it "decodes jsonb with version byte 0x01" $ do
      let jsonBytes = pgEncode (String "test")
          jsonbBytes = BS.singleton 1 <> jsonBytes
      pgDecode jsonbBytes `shouldBe` Right (String "test")

    it "decodes jsonb object with version byte" $ do
      let jsonBytes = pgEncode (object ["k" .= String "v"])
          jsonbBytes = BS.singleton 1 <> jsonBytes
      pgDecode jsonbBytes `shouldBe` Right (object ["k" .= String "v"])

  describe "JSON decode errors" $ do
    it "fails on invalid JSON" $ do
      let result = pgDecode "not json {{{" :: Either String Value
      result `shouldSatisfy` isLeft

    it "fails on empty input" $ do
      let result = pgDecode BS.empty :: Either String Value
      result `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

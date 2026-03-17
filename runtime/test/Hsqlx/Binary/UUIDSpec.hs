{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Hsqlx.Binary.UUIDSpec (spec) where

import Data.ByteString qualified as BS
import Data.UUID.Types qualified as UUID
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import Hsqlx.Binary.UUID ()
import Test.Hspec

spec :: Spec
spec = do
  describe "UUID round-trip" $ do
    it "nil UUID" $ do
      let u = UUID.nil
      pgDecode (pgEncode u) `shouldBe` Right u

    it "known UUID" $ do
      let Just u = UUID.fromString "550e8400-e29b-41d4-a716-446655440000"
      pgDecode (pgEncode u) `shouldBe` Right u

    it "another UUID" $ do
      let Just u = UUID.fromString "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
      pgDecode (pgEncode u) `shouldBe` Right u

  describe "UUID encode" $ do
    it "produces exactly 16 bytes" $ do
      let Just u = UUID.fromString "550e8400-e29b-41d4-a716-446655440000"
      BS.length (pgEncode u) `shouldBe` 16

    it "nil UUID encodes to 16 zero bytes" $
      pgEncode UUID.nil `shouldBe` BS.replicate 16 0

  describe "UUID decode errors" $ do
    it "fails on wrong byte count (15 bytes)" $ do
      let result = pgDecode (BS.replicate 15 0) :: Either String UUID.UUID
      result `shouldSatisfy` isLeft

    it "fails on wrong byte count (17 bytes)" $ do
      let result = pgDecode (BS.replicate 17 0) :: Either String UUID.UUID
      result `shouldSatisfy` isLeft

    it "fails on empty input" $ do
      let result = pgDecode BS.empty :: Either String UUID.UUID
      result `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

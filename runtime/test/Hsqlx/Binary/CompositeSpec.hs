module Hsqlx.Binary.CompositeSpec (spec) where

import Data.ByteString qualified as BS
import Hsqlx.Binary.Composite
import Hsqlx.Binary.Decode ()
import Hsqlx.Binary.Encode ()
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import Data.Int (Int32)
import Test.Hspec

spec :: Spec
spec = do
  describe "composite round-trip" $ do
    it "empty composite (no fields)" $ do
      let fields = [] :: [CompositeField]
      pgDecodeComposite (pgEncodeComposite fields) `shouldBe` Right fields

    it "single non-null field" $ do
      let val = pgEncode (42 :: Int32)
          fields = [CompositeField 23 (Just val)]
      pgDecodeComposite (pgEncodeComposite fields) `shouldBe` Right fields

    it "single null field" $ do
      let fields = [CompositeField 23 Nothing]
      pgDecodeComposite (pgEncodeComposite fields) `shouldBe` Right fields

    it "multiple fields with mixed nulls" $ do
      let fields =
            [ CompositeField 23 (Just (pgEncode (1 :: Int32)))
            , CompositeField 25 Nothing
            , CompositeField 23 (Just (pgEncode (99 :: Int32)))
            ]
      pgDecodeComposite (pgEncodeComposite fields) `shouldBe` Right fields

    it "preserves field OIDs" $ do
      let fields =
            [ CompositeField 23 (Just (pgEncode (1 :: Int32)))
            , CompositeField 25 (Just (pgEncode ("hello" :: BS.ByteString)))
            ]
          Right decoded = pgDecodeComposite (pgEncodeComposite fields)
      map cfOid decoded `shouldBe` [23, 25]

  describe "composite decode errors" $ do
    it "fails on empty input" $ do
      let result = pgDecodeComposite BS.empty
      result `shouldSatisfy` isLeft

    it "fails on truncated input" $ do
      let result = pgDecodeComposite (BS.pack [0, 0, 0, 2, 0])
      result `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

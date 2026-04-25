module Valiant.Binary.RefineSpec (spec) where

import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
import Valiant.Binary.Decode (refine, refineWith)
import Valiant.Binary.Encode ()
import PgWire.Binary.Types (PgEncode (..))
import Test.Hspec

newtype PositiveInt = PositiveInt Int32
  deriving (Eq, Show)

mkPositive :: Int32 -> Either String PositiveInt
mkPositive n
  | n > 0 = Right (PositiveInt n)
  | otherwise = Left ("expected positive, got " <> show n)

newtype Email = Email Text
  deriving (Eq, Show)

mkEmail :: Text -> Either String Email
mkEmail t
  | T.any (== '@') t = Right (Email t)
  | otherwise = Left "missing '@'"

spec :: Spec
spec = do
  describe "refine" $ do
    it "passes through valid values" $
      refine mkPositive (pgEncode (42 :: Int32))
        `shouldBe` Right (PositiveInt 42)

    it "fails when the predicate rejects" $
      refine mkPositive (pgEncode (0 :: Int32))
        `shouldBe` (Left "expected positive, got 0" :: Either String PositiveInt)

    it "fails when the underlying decode fails" $
      refine mkPositive (BS.pack [0, 0])
        `shouldBe` (Left "int32: expected 4 bytes, got 2" :: Either String PositiveInt)

    it "works for Text refinements" $
      refine mkEmail (pgEncode ("a@b" :: Text))
        `shouldBe` Right (Email "a@b")

  describe "refineWith" $ do
    it "prepends context to predicate failures" $
      refineWith "PositiveInt" mkPositive (pgEncode (-1 :: Int32))
        `shouldBe` (Left "PositiveInt: expected positive, got -1" :: Either String PositiveInt)

    it "prepends context to decode failures" $
      refineWith "PositiveInt" mkPositive (BS.pack [0])
        `shouldBe` (Left "PositiveInt: int32: expected 4 bytes, got 1" :: Either String PositiveInt)

    it "passes valid values unchanged" $
      refineWith "Email" mkEmail (pgEncode ("x@y" :: Text))
        `shouldBe` Right (Email "x@y")

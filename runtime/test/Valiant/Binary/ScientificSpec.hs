module Valiant.Binary.ScientificSpec (spec) where

import Data.Scientific (Scientific, scientific)
import Valiant.Binary.Scientific ()
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "numeric round-trip" $ do
    it "zero" $ do
      roundTrip (0 :: Scientific) `shouldBe` Right 0

    it "positive integer (42)" $ do
      roundTrip (42 :: Scientific) `shouldBe` Right 42

    it "negative integer (-100)" $ do
      roundTrip (-100 :: Scientific) `shouldBe` Right (-100)

    it "small decimal (3.14)" $ do
      roundTrip (3.14 :: Scientific) `shouldBe` Right 3.14

    it "negative decimal (-0.001)" $ do
      roundTrip (-0.001 :: Scientific) `shouldBe` Right (-0.001)

    it "large integer (1000000)" $ do
      roundTrip (1000000 :: Scientific) `shouldBe` Right 1000000

    it "scientific notation (1.23e10)" $ do
      roundTrip (scientific 123 8) `shouldBe` Right (scientific 123 8)

    it "very small (0.0001)" $ do
      roundTrip (0.0001 :: Scientific) `shouldBe` Right 0.0001

    it "one" $ do
      roundTrip (1 :: Scientific) `shouldBe` Right 1

    it "ten thousand" $ do
      roundTrip (10000 :: Scientific) `shouldBe` Right 10000

    it "9999" $ do
      roundTrip (9999 :: Scientific) `shouldBe` Right 9999

    it "negative one" $ do
      roundTrip (-1 :: Scientific) `shouldBe` Right (-1)

roundTrip :: Scientific -> Either String Scientific
roundTrip = pgDecode . pgEncode

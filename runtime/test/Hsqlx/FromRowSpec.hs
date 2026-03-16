module Hsqlx.FromRowSpec (spec) where

import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector qualified as V
import Hsqlx.Binary.Decode ()
import Hsqlx.Binary.Encode ()
import Hsqlx.Binary.Types (PgEncode (..))
import Hsqlx.FromRow
import Test.Hspec

-- Helper: encode a value and wrap as Just
enc :: (PgEncode a) => a -> Maybe BS.ByteString
enc = Just . pgEncode

spec :: Spec
spec = do
  describe "2-tuple FromRow" $ do
    it "decodes (Int32, Text)" $ do
      let row = V.fromList [enc (42 :: Int32), enc ("hello" :: Text)]
      fromRow row `shouldBe` Right (42 :: Int32, "hello" :: Text)

    it "decodes (Bool, Int64)" $ do
      let row = V.fromList [enc True, enc (999 :: Int64)]
      fromRow row `shouldBe` Right (True, 999 :: Int64)

  describe "3-tuple FromRow" $ do
    it "decodes (Int32, Text, Bool)" $ do
      let row = V.fromList [enc (1 :: Int32), enc ("alice" :: Text), enc True]
      fromRow row `shouldBe` Right (1 :: Int32, "alice" :: Text, True)

    it "decodes (Int64, Int32, Text)" $ do
      let row = V.fromList [enc (100 :: Int64), enc (42 :: Int32), enc ("test" :: Text)]
      fromRow row `shouldBe` Right (100 :: Int64, 42 :: Int32, "test" :: Text)

  describe "4-tuple FromRow" $ do
    it "decodes (Int32, Text, Bool, Int64)" $ do
      let row = V.fromList
            [ enc (1 :: Int32)
            , enc ("bob" :: Text)
            , enc False
            , enc (9999 :: Int64)
            ]
      fromRow row `shouldBe` Right (1 :: Int32, "bob" :: Text, False, 9999 :: Int64)

  describe "5-tuple FromRow" $ do
    it "decodes (Int32, Text, Int64, Bool, Double)" $ do
      let row = V.fromList
            [ enc (10 :: Int32)
            , enc ("carol" :: Text)
            , enc (500 :: Int64)
            , enc True
            , enc (3.14 :: Double)
            ]
      fromRow row `shouldBe` Right (10 :: Int32, "carol" :: Text, 500 :: Int64, True, 3.14 :: Double)

  describe "Column index out of range" $ do
    it "errors when row has fewer columns than expected (2-tuple with 1 column)" $ do
      let row = V.fromList [enc (42 :: Int32)]
          result = fromRow row :: Either String (Int32, Text)
      result `shouldSatisfy` isLeft

    it "errors when row has 0 columns for a 2-tuple" $ do
      let row = V.empty
          result = fromRow row :: Either String (Int32, Text)
      result `shouldSatisfy` isLeft

    it "errors when row has 2 columns for a 3-tuple" $ do
      let row = V.fromList [enc (1 :: Int32), enc ("x" :: Text)]
          result = fromRow row :: Either String (Int32, Text, Bool)
      result `shouldSatisfy` isLeft

  describe "NULL for non-nullable column" $ do
    it "errors when a non-Maybe column is NULL (first column)" $ do
      let row = V.fromList [Nothing, enc ("hello" :: Text)]
          result = fromRow row :: Either String (Int32, Text)
      result `shouldSatisfy` isLeft

    it "errors when a non-Maybe column is NULL (second column)" $ do
      let row = V.fromList [enc (42 :: Int32), Nothing]
          result = fromRow row :: Either String (Int32, Text)
      result `shouldSatisfy` isLeft

    it "error message mentions NULL" $ do
      let row = V.fromList [Nothing, enc ("hello" :: Text)]
          Left msg = fromRow row :: Either String (Int32, Text)
      msg `shouldSatisfy` \s -> "NULL" `isInfixOfStr` s

  describe "6-tuple FromRow" $ do
    it "decodes (Int32, Text, Bool, Int64, Double, Int32)" $ do
      let row = V.fromList
            [ enc (1 :: Int32)
            , enc ("test" :: Text)
            , enc True
            , enc (999 :: Int64)
            , enc (2.71 :: Double)
            , enc (7 :: Int32)
            ]
      fromRow row `shouldBe` Right (1 :: Int32, "test" :: Text, True, 999 :: Int64, 2.71 :: Double, 7 :: Int32)

-- Helpers
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isInfixOfStr :: String -> String -> Bool
isInfixOfStr needle haystack = any (isPrefixOfStr needle) (tails haystack)

isPrefixOfStr :: String -> String -> Bool
isPrefixOfStr [] _ = True
isPrefixOfStr _ [] = False
isPrefixOfStr (x : xs) (y : ys) = x == y && isPrefixOfStr xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_ : xs') = xs : tails xs'

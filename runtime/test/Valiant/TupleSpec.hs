module Valiant.TupleSpec (spec) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Valiant.FromRow (fromRow)
import Valiant.ToParams (toParams)
import Test.Hspec

-- | Round-trip each tuple size through ToParams (encode) then FromRow
-- (decode). Covers sizes 7-16; sizes 2-6 are covered by FromRowSpec.
-- The encoded form is @Vector (Maybe ByteString)@, which both classes
-- share, so toParams >>> fromRow should be Right . id.
spec :: Spec
spec = do
  describe "7-tuple round trip" $
    it "encodes and decodes back to the original" $ do
      let t = (1 :: Int32, "a" :: Text, True, 2 :: Int64, 3.5 :: Double, 4 :: Int32, "b" :: Text)
      fromRow (toParams t) `shouldBe` Right t

  describe "8-tuple round trip" $
    it "encodes and decodes back to the original" $ do
      let t = (1 :: Int32, "a" :: Text, True, 2 :: Int64, 3.5 :: Double, 4 :: Int32, "b" :: Text, False)
      fromRow (toParams t) `shouldBe` Right t

  describe "9-tuple round trip" $
    it "encodes and decodes back to the original" $ do
      let t =
            ( 1 :: Int32, "a" :: Text, True, 2 :: Int64, 3.5 :: Double
            , 4 :: Int32, "b" :: Text, False, 5 :: Int64
            )
      fromRow (toParams t) `shouldBe` Right t

  describe "10-tuple round trip" $
    it "encodes and decodes back to the original" $ do
      let t =
            ( 1 :: Int32, "a" :: Text, True, 2 :: Int64, 3.5 :: Double
            , 4 :: Int32, "b" :: Text, False, 5 :: Int64, 6.25 :: Double
            )
      fromRow (toParams t) `shouldBe` Right t

  describe "11-tuple round trip" $
    it "encodes and decodes back to the original" $ do
      let t =
            ( 1 :: Int32, "a" :: Text, True, 2 :: Int64, 3.5 :: Double
            , 4 :: Int32, "b" :: Text, False, 5 :: Int64, 6.25 :: Double
            , 7 :: Int32
            )
      fromRow (toParams t) `shouldBe` Right t

  describe "12-tuple round trip" $
    it "encodes and decodes back to the original" $ do
      let t =
            ( 1 :: Int32, "a" :: Text, True, 2 :: Int64, 3.5 :: Double
            , 4 :: Int32, "b" :: Text, False, 5 :: Int64, 6.25 :: Double
            , 7 :: Int32, "c" :: Text
            )
      fromRow (toParams t) `shouldBe` Right t

  describe "13-tuple round trip" $
    it "encodes and decodes back to the original" $ do
      let t =
            ( 1 :: Int32, "a" :: Text, True, 2 :: Int64, 3.5 :: Double
            , 4 :: Int32, "b" :: Text, False, 5 :: Int64, 6.25 :: Double
            , 7 :: Int32, "c" :: Text, True
            )
      fromRow (toParams t) `shouldBe` Right t

  describe "14-tuple round trip" $
    it "encodes and decodes back to the original" $ do
      let t =
            ( 1 :: Int32, "a" :: Text, True, 2 :: Int64, 3.5 :: Double
            , 4 :: Int32, "b" :: Text, False, 5 :: Int64, 6.25 :: Double
            , 7 :: Int32, "c" :: Text, True, 8 :: Int64
            )
      fromRow (toParams t) `shouldBe` Right t

  describe "15-tuple round trip" $
    it "encodes and decodes back to the original" $ do
      let t =
            ( 1 :: Int32, "a" :: Text, True, 2 :: Int64, 3.5 :: Double
            , 4 :: Int32, "b" :: Text, False, 5 :: Int64, 6.25 :: Double
            , 7 :: Int32, "c" :: Text, True, 8 :: Int64, 9.75 :: Double
            )
      fromRow (toParams t) `shouldBe` Right t

  -- base provides Show and Eq only up to 15-tuples, so 16-tuple
  -- round trips are checked by destructuring the result instead of
  -- using shouldBe on the whole value.
  describe "16-tuple round trip" $
    it "encodes and decodes back to the original" $ do
      let t =
            ( 1 :: Int32, "a" :: Text, True, 2 :: Int64, 3.5 :: Double
            , 4 :: Int32, "b" :: Text, False, 5 :: Int64, 6.25 :: Double
            , 7 :: Int32, "c" :: Text, True, 8 :: Int64, 9.75 :: Double
            , 10 :: Int32
            )
      case fromRow (toParams t) of
        Left err -> expectationFailure err
        Right (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> do
          (a, b, c, d, e, f, g, h) `shouldBe`
            (1 :: Int32, "a" :: Text, True, 2 :: Int64, 3.5 :: Double, 4 :: Int32, "b" :: Text, False)
          (i, j, k, l, m, n, o, p) `shouldBe`
            (5 :: Int64, 6.25 :: Double, 7 :: Int32, "c" :: Text, True, 8 :: Int64, 9.75 :: Double, 10 :: Int32)

  describe "16-tuple with NULLs via Maybe" $
    it "round trips with Nothing mixed in" $ do
      let t =
            ( Just (1 :: Int32), Nothing :: Maybe Text, True, 2 :: Int64, 3.5 :: Double
            , Nothing :: Maybe Int32, "b" :: Text, False, Just (5 :: Int64), 6.25 :: Double
            , 7 :: Int32, Nothing :: Maybe Text, True, 8 :: Int64, Nothing :: Maybe Double
            , 10 :: Int32
            )
      case fromRow (toParams t) of
        Left err -> expectationFailure err
        Right (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> do
          (a, b, c, d, e, f, g, h) `shouldBe`
            (Just (1 :: Int32), Nothing :: Maybe Text, True, 2 :: Int64, 3.5 :: Double, Nothing :: Maybe Int32, "b" :: Text, False)
          (i, j, k, l, m, n, o, p) `shouldBe`
            (Just (5 :: Int64), 6.25 :: Double, 7 :: Int32, Nothing :: Maybe Text, True, 8 :: Int64, Nothing :: Maybe Double, 10 :: Int32)

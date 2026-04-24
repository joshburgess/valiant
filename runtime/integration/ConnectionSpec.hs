module ConnectionSpec (spec) where

import Valiant
import TestSupport
import Test.Hspec

spec :: Spec
spec = do
  describe "connectString" $ do
    it "connects and disconnects cleanly" $ do
      withTestConnection $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT 1"
        length rows `shouldBe` 1

    it "returns server version via parameter status" $ do
      withTestConnection $ \conn -> do
        (rows, _) <- simpleQuery conn "SHOW server_version"
        length rows `shouldBe` 1

  describe "simpleQuery" $ do
    it "handles empty query" $ do
      withTestConnection $ \conn -> do
        (rows, _) <- simpleQuery conn ""
        rows `shouldBe` []

    it "returns multiple rows" $ do
      withTestConnection $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT generate_series(1, 5)"
        length rows `shouldBe` 5

    it "returns NULL values" $ do
      withTestConnection $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT NULL::text"
        length rows `shouldBe` 1
        case rows of
          [[Nothing]] -> pure ()
          _ -> expectationFailure $ "Expected [[Nothing]], got: " <> show rows

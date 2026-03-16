module PgWire.Pool.ConfigSpec (spec) where

import PgWire.Pool.Config
import Test.Hspec

spec :: Spec
spec = do
  describe "defaultPoolConfig" $ do
    it "has empty connection string" $
      poolConnString defaultPoolConfig `shouldBe` ""

    it "has pool size 10" $
      poolSize defaultPoolConfig `shouldBe` 10

    it "has idle time 600 seconds" $
      poolIdleTime defaultPoolConfig `shouldBe` 600

    it "has max life 3600 seconds" $
      poolMaxLife defaultPoolConfig `shouldBe` 3600

    it "has acquire timeout 10 seconds" $
      poolAcquireTimeout defaultPoolConfig `shouldBe` 10

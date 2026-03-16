module Hsqlx.Plugin.ConfigSpec (spec) where

import Hsqlx.Plugin.Config
import Test.Hspec

spec :: Spec
spec = do
  describe "parseOptions" $ do
    it "returns defaults for empty options" $ do
      let cfg = parseOptions []
      pcSqlDir cfg `shouldBe` "sql"
      pcCacheDir cfg `shouldBe` ".hsqlx"
      pcOffline cfg `shouldBe` False

    it "parses sql-dir" $ do
      let cfg = parseOptions ["sql-dir=queries"]
      pcSqlDir cfg `shouldBe` "queries"

    it "parses cache-dir" $ do
      let cfg = parseOptions ["cache-dir=.cache"]
      pcCacheDir cfg `shouldBe` ".cache"

    it "parses offline=true" $ do
      let cfg = parseOptions ["offline=true"]
      pcOffline cfg `shouldBe` True

    it "parses offline=false" $ do
      let cfg = parseOptions ["offline=false"]
      pcOffline cfg `shouldBe` False

    it "handles multiple options" $ do
      let cfg = parseOptions ["sql-dir=q", "cache-dir=c", "offline=true"]
      pcSqlDir cfg `shouldBe` "q"
      pcCacheDir cfg `shouldBe` "c"
      pcOffline cfg `shouldBe` True

    it "ignores unknown options" $ do
      let cfg = parseOptions ["unknown=value", "sql-dir=q"]
      pcSqlDir cfg `shouldBe` "q"
      pcCacheDir cfg `shouldBe` ".hsqlx"

module Valiant.CLI.SqlMetadataSpec (spec) where

import Valiant.CLI.SqlMetadata
import Test.Hspec

spec :: Spec
spec = do
  describe "parseSqlMetadata" $ do
    it "parses empty SQL as default metadata" $ do
      let meta = parseSqlMetadata "SELECT 1"
      smName meta `shouldBe` Nothing
      smResult meta `shouldBe` Nothing
      smSingle meta `shouldBe` False

    it "parses -- valiant:name directive" $ do
      let sql = "-- valiant:name getUserById\nSELECT id, name FROM users WHERE id = $1"
          meta = parseSqlMetadata sql
      smName meta `shouldBe` Just "getUserById"

    it "parses -- valiant:result directive" $ do
      let sql = "-- valiant:result User\nSELECT id, name FROM users WHERE id = $1"
          meta = parseSqlMetadata sql
      smResult meta `shouldBe` Just "User"

    it "parses -- valiant:single directive" $ do
      let sql = "-- valiant:single\nSELECT id, name FROM users WHERE id = $1"
          meta = parseSqlMetadata sql
      smSingle meta `shouldBe` True

    it "parses multiple directives" $ do
      let sql =
            "-- valiant:name getUserById\n\
            \-- valiant:result User\n\
            \-- valiant:single\n\
            \SELECT id, name FROM users WHERE id = $1"
          meta = parseSqlMetadata sql
      smName meta `shouldBe` Just "getUserById"
      smResult meta `shouldBe` Just "User"
      smSingle meta `shouldBe` True

    it "ignores regular SQL comments" $ do
      let sql = "-- This is a regular comment\nSELECT 1"
          meta = parseSqlMetadata sql
      smName meta `shouldBe` Nothing

    it "handles leading whitespace on directive lines" $ do
      let sql = "  -- valiant:name myQuery\nSELECT 1"
          meta = parseSqlMetadata sql
      smName meta `shouldBe` Just "myQuery"

    it "ignores directives with empty values for name and result" $ do
      let sql = "-- valiant:name\nSELECT 1"
          meta = parseSqlMetadata sql
      smName meta `shouldBe` Nothing

    it "ignores unknown directive keys" $ do
      let sql = "-- valiant:foobar baz\nSELECT 1"
          meta = parseSqlMetadata sql
      meta `shouldBe` defaultMetadata

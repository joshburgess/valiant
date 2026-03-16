module Hsqlx.CLI.SqlMetadataSpec (spec) where

import Hsqlx.CLI.SqlMetadata
import Test.Hspec

spec :: Spec
spec = do
  describe "parseSqlMetadata" $ do
    it "parses empty SQL as default metadata" $ do
      let meta = parseSqlMetadata "SELECT 1"
      smName meta `shouldBe` Nothing
      smResult meta `shouldBe` Nothing
      smSingle meta `shouldBe` False

    it "parses -- hsqlx:name directive" $ do
      let sql = "-- hsqlx:name getUserById\nSELECT id, name FROM users WHERE id = $1"
          meta = parseSqlMetadata sql
      smName meta `shouldBe` Just "getUserById"

    it "parses -- hsqlx:result directive" $ do
      let sql = "-- hsqlx:result User\nSELECT id, name FROM users WHERE id = $1"
          meta = parseSqlMetadata sql
      smResult meta `shouldBe` Just "User"

    it "parses -- hsqlx:single directive" $ do
      let sql = "-- hsqlx:single\nSELECT id, name FROM users WHERE id = $1"
          meta = parseSqlMetadata sql
      smSingle meta `shouldBe` True

    it "parses multiple directives" $ do
      let sql =
            "-- hsqlx:name getUserById\n\
            \-- hsqlx:result User\n\
            \-- hsqlx:single\n\
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
      let sql = "  -- hsqlx:name myQuery\nSELECT 1"
          meta = parseSqlMetadata sql
      smName meta `shouldBe` Just "myQuery"

    it "ignores directives with empty values for name and result" $ do
      let sql = "-- hsqlx:name\nSELECT 1"
          meta = parseSqlMetadata sql
      smName meta `shouldBe` Nothing

    it "ignores unknown directive keys" $ do
      let sql = "-- hsqlx:foobar baz\nSELECT 1"
          meta = parseSqlMetadata sql
      meta `shouldBe` defaultMetadata

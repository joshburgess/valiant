module Hsqlx.CLI.NamedParamsSpec (spec) where

import Data.ByteString.Char8 qualified as BS8
import Hsqlx.CLI.NamedParams
import Test.Hspec

spec :: Spec
spec = do
  describe "preprocessNamedParams" $ do
    -- Basic functionality
    it "replaces a single :name with $1" $ do
      let (sql, mapping) = preprocessNamedParams "SELECT * FROM users WHERE id = :userId"
      sql `shouldBe` "SELECT * FROM users WHERE id = $1"
      mapping `shouldBe` [("userId", 1)]

    it "replaces multiple distinct :names" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT * FROM users WHERE org_id = :orgId AND role = :role AND active = :active"
      sql `shouldBe` "SELECT * FROM users WHERE org_id = $1 AND role = $2 AND active = $3"
      mapping `shouldBe` [("active", 3), ("orgId", 1), ("role", 2)]

    it "reuses the same $N for repeated :name" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT * FROM t WHERE a = :x OR b = :x"
      sql `shouldBe` "SELECT * FROM t WHERE a = $1 OR b = $1"
      mapping `shouldBe` [("x", 1)]

    it "handles mixed repeated and distinct names" $ do
      let (sql, _mapping) = preprocessNamedParams
            "SELECT * FROM t WHERE a = :x AND b = :y AND c = :x"
      sql `shouldBe` "SELECT * FROM t WHERE a = $1 AND b = $2 AND c = $1"

    -- No-op cases
    it "returns original SQL unchanged when no named params" $ do
      let input = "SELECT * FROM users WHERE id = $1"
      let (sql, mapping) = preprocessNamedParams input
      sql `shouldBe` input
      mapping `shouldBe` []

    it "returns empty mapping for parameterless queries" $ do
      let (sql, mapping) = preprocessNamedParams "SELECT 1"
      sql `shouldBe` "SELECT 1"
      mapping `shouldBe` []

    -- Postgres cast (::) handling
    it "does not treat :: as a named parameter" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT :val::text FROM t WHERE id = :id"
      sql `shouldBe` "SELECT $1::text FROM t WHERE id = $2"
      mapping `shouldBe` [("id", 2), ("val", 1)]

    it "handles multiple casts" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT :a::int4, :b::text"
      sql `shouldBe` "SELECT $1::int4, $2::text"
      mapping `shouldBe` [("a", 1), ("b", 2)]

    it "handles cast without named param" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT $1::text"
      sql `shouldBe` "SELECT $1::text"
      mapping `shouldBe` []

    -- String literal handling
    it "does not substitute inside string literals" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT * FROM t WHERE name = :name AND note = ':not_a_param'"
      sql `shouldBe` "SELECT * FROM t WHERE name = $1 AND note = ':not_a_param'"
      mapping `shouldBe` [("name", 1)]

    it "handles escaped single quotes in strings" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT * FROM t WHERE name = 'it''s :not_a_param' AND id = :id"
      sql `shouldBe` "SELECT * FROM t WHERE name = 'it''s :not_a_param' AND id = $1"
      mapping `shouldBe` [("id", 1)]

    -- Quoted identifier handling
    it "does not substitute inside quoted identifiers" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT \":notParam\" FROM t WHERE id = :id"
      sql `shouldBe` "SELECT \":notParam\" FROM t WHERE id = $1"
      mapping `shouldBe` [("id", 1)]

    -- Comment handling
    it "does not substitute inside line comments" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT * FROM t -- WHERE :name = 'foo'\nWHERE id = :id"
      sql `shouldBe` "SELECT * FROM t -- WHERE :name = 'foo'\nWHERE id = $1"
      mapping `shouldBe` [("id", 1)]

    it "does not substitute inside block comments" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT * /* :notParam */ FROM t WHERE id = :id"
      sql `shouldBe` "SELECT * /* :notParam */ FROM t WHERE id = $1"
      mapping `shouldBe` [("id", 1)]

    -- Naming conventions
    it "supports underscored parameter names" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT * FROM t WHERE org_id = :org_id"
      sql `shouldBe` "SELECT * FROM t WHERE org_id = $1"
      mapping `shouldBe` [("org_id", 1)]

    it "supports names starting with underscore" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT * FROM t WHERE id = :_internal"
      sql `shouldBe` "SELECT * FROM t WHERE id = $1"
      mapping `shouldBe` [("_internal", 1)]

    it "supports names with digits" $ do
      let (sql, mapping) = preprocessNamedParams
            "SELECT * FROM t WHERE id = :param1 AND val = :param2"
      sql `shouldBe` "SELECT * FROM t WHERE id = $1 AND val = $2"
      mapping `shouldBe` [("param1", 1), ("param2", 2)]

    -- Edge cases
    it "handles :name at end of input" $ do
      let (sql, mapping) = preprocessNamedParams "SELECT * FROM t WHERE id = :id"
      sql `shouldBe` "SELECT * FROM t WHERE id = $1"
      mapping `shouldBe` [("id", 1)]

    it "handles :name at start of input" $ do
      let (sql, mapping) = preprocessNamedParams ":val"
      sql `shouldBe` "$1"
      mapping `shouldBe` [("val", 1)]

    it "handles adjacent :names" $ do
      let (sql, _) = preprocessNamedParams "VALUES (:a,:b,:c)"
      sql `shouldBe` "VALUES ($1,$2,$3)"

    it "does not treat bare colon as param" $ do
      let (sql, mapping) = preprocessNamedParams "SELECT * FROM t WHERE time > '12:30:00'"
      sql `shouldBe` "SELECT * FROM t WHERE time > '12:30:00'"
      mapping `shouldBe` []

    it "does not treat colon followed by digit as param" $ do
      let (sql, mapping) = preprocessNamedParams "SELECT * FROM t WHERE x = :1bad"
      -- :1 is not a valid param name (starts with digit)
      sql `shouldBe` "SELECT * FROM t WHERE x = :1bad"
      mapping `shouldBe` []

    -- Realistic queries
    it "handles a realistic INSERT" $ do
      let (sql, mapping) = preprocessNamedParams
            "INSERT INTO users (name, email, org_id) VALUES (:name, :email, :orgId)"
      sql `shouldBe` "INSERT INTO users (name, email, org_id) VALUES ($1, $2, $3)"
      length mapping `shouldBe` 3

    it "handles a realistic UPDATE" $ do
      let (sql, mapping) = preprocessNamedParams
            "UPDATE users SET name = :name, email = :email WHERE id = :userId"
      sql `shouldBe` "UPDATE users SET name = $1, email = $2 WHERE id = $3"
      length mapping `shouldBe` 3

    it "handles a CTE with named params" $ do
      let (sql, _) = preprocessNamedParams
            "WITH active AS (SELECT * FROM users WHERE active = :active)\n\
            \SELECT * FROM active WHERE org_id = :orgId"
      BS8.isInfixOf "$1" sql `shouldBe` True
      BS8.isInfixOf "$2" sql `shouldBe` True
      BS8.isInfixOf ":active" sql `shouldBe` False
      BS8.isInfixOf ":orgId" sql `shouldBe` False

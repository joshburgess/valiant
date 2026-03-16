module Hsqlx.CLI.CacheSpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Hsqlx.CLI.Cache
import Test.Hspec

spec :: Spec
spec = do
  describe "StatementType JSON round-trip" $ do
    it "round-trips Select" $ do
      eitherDecode (encode Select) `shouldBe` Right Select

    it "round-trips Insert" $ do
      eitherDecode (encode Insert) `shouldBe` Right Insert

  describe "CacheEntry JSON round-trip" $ do
    it "round-trips a full cache entry" $ do
      now <- getCurrentTime
      let entry = sampleEntry now
      eitherDecode (encode entry) `shouldBe` Right entry

  describe "cacheFileName" $ do
    it "produces the expected format" $ do
      cacheFileName "users/find_by_id.sql" "a1b2c3d4e5f6"
        `shouldBe` "users-find_by_id-a1b2c3d4e5f6.json"

    it "handles nested paths" $ do
      let name = cacheFileName "admin/reports/monthly.sql" "abcdef012345"
      name `shouldBe` "admin-reports-monthly-abcdef012345.json"

  describe "statementTypeFromSql" $ do
    it "detects SELECT" $ do
      statementTypeFromSql "SELECT id FROM users" `shouldBe` Select

    it "detects INSERT" $ do
      statementTypeFromSql "INSERT INTO users (name) VALUES ($1)" `shouldBe` Insert

    it "detects UPDATE" $ do
      statementTypeFromSql "UPDATE users SET name = $1" `shouldBe` Update

    it "detects DELETE" $ do
      statementTypeFromSql "DELETE FROM users WHERE id = $1" `shouldBe` Delete

    it "detects CTE as Select" $ do
      statementTypeFromSql "WITH cte AS (SELECT 1) SELECT * FROM cte" `shouldBe` Select

    it "is case-insensitive" $ do
      statementTypeFromSql "select id from users" `shouldBe` Select

    it "handles leading whitespace" $ do
      statementTypeFromSql "  \n  SELECT id FROM users" `shouldBe` Select

sampleEntry :: UTCTime -> CacheEntry
sampleEntry now =
  CacheEntry
    { ceVersion = "0.1.0"
    , ceFile = "users/find_by_id.sql"
    , ceSqlHash = "abcdef0123456789"
    , ceSql = "SELECT id, name FROM users WHERE id = $1"
    , ceDbUrlHash = "fedcba9876543210"
    , cePreparedAt = now
    , ceStatementType = Select
    , ceParams =
        [ CacheParam
            { cpIndex = 1
            , cpPgOid = 23
            , cpPgTypeName = "int4"
            , cpHaskellType = "Int32"
            , cpHaskellModule = "Data.Int"
            }
        ]
    , ceColumns =
        [ CacheColumn
            { ccName = "id"
            , ccPgOid = 23
            , ccPgTypeName = "int4"
            , ccNullable = False
            , ccHaskellType = "Int32"
            , ccHaskellModule = "Data.Int"
            , ccSourceTableOid = Just 16385
            , ccSourceColumnNum = Just 1
            }
        , CacheColumn
            { ccName = "name"
            , ccPgOid = 25
            , ccPgTypeName = "text"
            , ccNullable = False
            , ccHaskellType = "Text"
            , ccHaskellModule = "Data.Text"
            , ccSourceTableOid = Just 16385
            , ccSourceColumnNum = Just 2
            }
        ]
    }

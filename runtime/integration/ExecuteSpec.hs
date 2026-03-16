module ExecuteSpec (spec) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Hsqlx
import TestSupport
import Test.Hspec

stmtSelectOne :: Statement Int32 (Int32, Text, Maybe Text)
stmtSelectOne = mkStatement
  "SELECT id, name, email FROM users WHERE id = $1"
  [23] ["id", "name", "email"] "<test>"

stmtListAll :: Statement () (Int32, Text)
stmtListAll = mkStatement
  "SELECT id, name FROM users ORDER BY id"
  [] ["id", "name"] "<test>"

stmtInsert :: Statement (Text, Maybe Text) ()
stmtInsert = mkStatement
  "INSERT INTO users (name, email) VALUES ($1, $2)"
  [25, 25] [] "<test>"

stmtCount :: Statement () Int64
stmtCount = mkStatement
  "SELECT count(*) FROM users"
  [] ["count"] "<test>"

spec :: Spec
spec = do
  describe "fetchOne" $ do
    it "returns Nothing for missing row" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        result <- fetchOne conn stmtSelectOne 999
        result `shouldBe` Nothing

    it "returns Just for existing row" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        result <- fetchOne conn stmtSelectOne 1
        case result of
          Just (_, name, email) -> do
            name `shouldBe` "Alice"
            email `shouldBe` Just "alice@example.com"
          Nothing -> expectationFailure "Expected Just, got Nothing"

  describe "fetchAll" $ do
    it "returns empty list for no rows" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        rows <- fetchAll conn stmtListAll ()
        rows `shouldBe` []

    it "returns all rows" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        rows <- fetchAll conn stmtListAll ()
        length rows `shouldBe` 5

  describe "execute" $ do
    it "inserts a row and returns rows affected" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        n <- execute conn stmtInsert ("TestUser", Just "test@example.com")
        n `shouldBe` 1

  describe "fetchScalar" $ do
    it "returns a scalar count" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        count <- fetchScalar conn stmtCount ()
        count `shouldBe` 5

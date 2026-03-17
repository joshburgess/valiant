module ExecuteSpec (spec) where

import Control.Exception (try)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Hsqlx
import PgWire.Error (HsqlxError (..))
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

stmtInsertReturningId :: Statement (Text, Maybe Text) Int32
stmtInsertReturningId = mkStatement
  "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id"
  [25, 25] ["id"] "<test>"

stmtInsertMultiReturningId :: Statement () Int32
stmtInsertMultiReturningId = mkStatement
  "INSERT INTO users (name, email) VALUES ('X', 'x@test.com'), ('Y', 'y@test.com'), ('Z', 'z@test.com') RETURNING id"
  [] ["id"] "<test>"

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

  describe "executeReturning" $ do
    it "returns row count and decoded rows for single insert" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        (n, ids) <- executeReturning conn stmtInsertReturningId ("NewUser", Just "new@example.com")
        n `shouldBe` 1
        length ids `shouldBe` 1

    it "returns row count and decoded rows for multi-value insert" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        (n, ids) <- executeReturning conn stmtInsertMultiReturningId ()
        n `shouldBe` 3
        length ids `shouldBe` 3

  describe "fetchOneOrThrow" $ do
    it "returns the value for an existing row" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        (uid, name, email) <- fetchOneOrThrow conn stmtSelectOne 1
        name `shouldBe` "Alice"
        email `shouldBe` Just "alice@example.com"
        uid `shouldSatisfy` (> 0)

    it "throws on a missing row" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        result <- try (fetchOneOrThrow conn stmtSelectOne (-1))
        case result of
          Left (DecodeError _) -> pure ()
          Left err -> expectationFailure $ "Expected DecodeError, got: " <> show err
          Right _ -> expectationFailure "Expected an exception, but got a result"

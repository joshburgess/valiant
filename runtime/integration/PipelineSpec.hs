module PipelineSpec (spec) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Valiant
import PgWire.Protocol.Oid (oidInt4)
import TestSupport
import Test.Hspec

stmtFetchOne :: Statement Int32 (Int32, Text, Maybe Text)
stmtFetchOne = mkStatement
  "SELECT id, name, email FROM users WHERE id = $1"
  [23] ["id", "name", "email"] "<test>"

stmtCount :: Statement () Int64
stmtCount = mkStatement
  "SELECT count(*) FROM users"
  [] ["count"] "<test>"

stmtListAll :: Statement () (Int32, Text)
stmtListAll = mkStatement
  "SELECT id, name FROM users ORDER BY id"
  [] ["id", "name"] "<test>"

spec :: Spec
spec = do
  describe "runPipeline" $ do
    it "executes multiple queries in one round-trip" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        (mUser, count) <- runPipeline conn $ (,)
          <$> pipeFetchOne stmtFetchOne 1
          <*> pipeFetchScalar stmtCount ()
        case mUser of
          Just (_, name, _) -> name `shouldBe` "Alice"
          Nothing -> expectationFailure "Expected user"
        count `shouldBe` 5

    it "handles mixed result types" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        (users, count, mMissing) <- runPipeline conn $ (,,)
          <$> pipeFetchAll stmtListAll ()
          <*> pipeFetchScalar stmtCount ()
          <*> pipeFetchOne stmtFetchOne 999
        length users `shouldBe` 5
        count `shouldBe` 5
        mMissing `shouldBe` Nothing

    it "handles empty pipeline" $ do
      withTestConnection $ \conn -> do
        result <- runPipeline conn (pure 42 :: Pipeline Int)
        result `shouldBe` 42

    it "handles single query" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        count <- runPipeline conn $ pipeFetchScalar stmtCount ()
        count `shouldBe` 5

  describe "fetchBatchOne" $ do
    it "fetches multiple rows by different params" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        results <- fetchBatchOne conn stmtFetchOne [1, 2, 3, 999]
        length results `shouldBe` 4
        -- First 3 should be Just, last should be Nothing
        case results of
          [Just (_, n1, _), Just (_, n2, _), Just (_, n3, _), Nothing] -> do
            n1 `shouldBe` "Alice"
            n2 `shouldBe` "Bob"
            n3 `shouldBe` "Carol"
          _ -> expectationFailure $ "Unexpected: " <> show (length results)

    it "handles empty list" $ do
      withTestConnection $ \conn -> do
        results <- fetchBatchOne conn stmtFetchOne ([] :: [Int32])
        results `shouldBe` []

  describe "fetchBatchAll" $ do
    it "fetches multiple result sets" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        results <- fetchBatchAll conn stmtListAll [(), ()]
        length results `shouldBe` 2
        case results of
          (r : _) -> length r `shouldBe` 5
          [] -> expectationFailure "Expected non-empty results"
        length (results !! 1) `shouldBe` 5

  describe "fetchByIds" $ do
    it "fetches multiple rows by array parameter" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        users <- fetchByIds conn
          "SELECT id, name, email FROM users WHERE id = ANY($1::int4[])"
          oidInt4
          [1 :: Int32, 2, 3]
        length (users :: [(Int32, Text, Maybe Text)]) `shouldBe` 3

    it "returns empty for no matches" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        users <- fetchByIds conn
          "SELECT id, name, email FROM users WHERE id = ANY($1::int4[])"
          oidInt4
          [999 :: Int32, 998]
        length (users :: [(Int32, Text, Maybe Text)]) `shouldBe` 0

    it "handles empty ID list" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        users <- fetchByIds conn
          "SELECT id, name, email FROM users WHERE id = ANY($1::int4[])"
          oidInt4
          ([] :: [Int32])
        length (users :: [(Int32, Text, Maybe Text)]) `shouldBe` 0

module PipelineSpec (spec) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Hsqlx
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

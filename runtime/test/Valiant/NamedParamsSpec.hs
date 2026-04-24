{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Valiant.NamedParamsSpec (spec) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Valiant.NamedParams
import Valiant.Statement (Statement (..))
import PgWire.Binary.Types (PgEncode (..))
import Test.Hspec

-- Test record types
data SimpleParams = SimpleParams
  { userId :: Int32
  }
  deriving stock (Generic)
  deriving anyclass (ToNamedParams)

data MultiParams = MultiParams
  { orgId :: Int32
  , role :: Text
  , active :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToNamedParams)

data NullableParams = NullableParams
  { name :: Text
  , email :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToNamedParams)

-- Record with fields that don't match any SQL param
data ExtraFieldParams = ExtraFieldParams
  { knownField :: Int32
  , unknownField :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToNamedParams)

spec :: Spec
spec = do
  describe "ToNamedParams (Generic)" $ do
    it "encodes a single-field record" $ do
      let pairs = toNamedParamList (SimpleParams 42)
      length pairs `shouldBe` 1
      case pairs of
        [(n, val)] -> do
          n `shouldBe` "userId"
          val `shouldBe` Just (pgEncode (42 :: Int32))
        _ -> expectationFailure "expected exactly one pair"

    it "encodes a multi-field record with correct names" $ do
      let pairs = toNamedParamList (MultiParams 1 "admin" True)
          names = map fst pairs
      names `shouldBe` ["orgId", "role", "active"]

    it "encodes a multi-field record with correct values" $ do
      let pairs = toNamedParamList (MultiParams 1 "admin" True)
      snd (pairs !! 0) `shouldBe` Just (pgEncode (1 :: Int32))
      snd (pairs !! 1) `shouldBe` Just (TE.encodeUtf8 "admin")
      snd (pairs !! 2) `shouldBe` Just (pgEncode True)

    it "encodes Nothing as NULL" $ do
      let pairs = toNamedParamList (NullableParams "Alice" Nothing)
      snd (pairs !! 0) `shouldBe` Just (TE.encodeUtf8 "Alice")
      snd (pairs !! 1) `shouldBe` Nothing

    it "encodes Just value correctly" $ do
      let pairs = toNamedParamList (NullableParams "Alice" (Just "alice@example.com"))
      snd (pairs !! 1) `shouldBe` Just (TE.encodeUtf8 "alice@example.com")

  describe "mkStatementNamed" $ do
    it "reorders fields to match positional parameter order" $ do
      let stmt = mkStatementNamed @MultiParams @()
            "SELECT 1 WHERE org_id = $1 AND active = $2 AND role = $3"
            [23, 16, 25]
            []
            ["orgId", "active", "role"]
            "test.sql"
          -- orgId=$1, active=$2, role=$3
          encoded = stmtEncode stmt (MultiParams 42 "admin" True)
      -- $1 = orgId = 42
      (V.!) encoded 0 `shouldBe` Just (pgEncode (42 :: Int32))
      -- $2 = active = True
      (V.!) encoded 1 `shouldBe` Just (pgEncode True)
      -- $3 = role = "admin"
      (V.!) encoded 2 `shouldBe` Just (TE.encodeUtf8 "admin")

    it "handles duplicate param names (record field used once)" $ do
      let stmt = mkStatementNamed @SimpleParams @()
            "SELECT 1 WHERE a = $1 OR b = $1"
            [23]
            []
            ["userId"]
            "test.sql"
          encoded = stmtEncode stmt (SimpleParams 99)
      V.length encoded `shouldBe` 1
      (V.!) encoded 0 `shouldBe` Just (pgEncode (99 :: Int32))

    it "preserves original field order in same-order case" $ do
      let stmt = mkStatementNamed @MultiParams @()
            "SELECT 1"
            [23, 25, 16]
            []
            ["orgId", "role", "active"]
            "test.sql"
          encoded = stmtEncode stmt (MultiParams 7 "viewer" False)
      (V.!) encoded 0 `shouldBe` Just (pgEncode (7 :: Int32))
      (V.!) encoded 1 `shouldBe` Just (TE.encodeUtf8 "viewer")
      (V.!) encoded 2 `shouldBe` Just (pgEncode False)

    it "handles nullable params in named mode" $ do
      let stmt = mkStatementNamed @NullableParams @()
            "INSERT INTO t (name, email) VALUES ($1, $2)"
            [25, 25]
            []
            ["name", "email"]
            "test.sql"
          encoded = stmtEncode stmt (NullableParams "Bob" Nothing)
      (V.!) encoded 0 `shouldBe` Just (TE.encodeUtf8 "Bob")
      (V.!) encoded 1 `shouldBe` Nothing

  describe "error messages" $ do
    it "reports missing record fields with helpful message" $ do
      let stmt = mkStatementNamed @SimpleParams @()
            "SELECT 1 WHERE org = $1 AND role = $2"
            [23, 25]
            []
            ["orgId", "role"]  -- record only has userId, not orgId or role
            "test.sql"
      result <- try @ErrorCall $ evaluate (stmtEncode stmt (SimpleParams 1))
      case result of
        Left (ErrorCall msg) -> do
          msg `shouldContain` "named parameter mismatch"
          msg `shouldContain` "Missing fields"
          msg `shouldContain` ":orgId"
          msg `shouldContain` ":role"
          msg `shouldContain` "userId"  -- listed as available
        Right _ -> expectationFailure "expected error for missing fields"

    it "reports extra record fields with helpful message" $ do
      let stmt = mkStatementNamed @ExtraFieldParams @()
            "SELECT 1 WHERE id = $1"
            [23]
            []
            ["knownField"]  -- SQL only uses knownField, not unknownField
            "test.sql"
      result <- try @ErrorCall $ evaluate (stmtEncode stmt (ExtraFieldParams 1 "extra"))
      case result of
        Left (ErrorCall msg) -> do
          msg `shouldContain` "named parameter mismatch"
          msg `shouldContain` "Extra fields"
          msg `shouldContain` "unknownField"
          msg `shouldContain` "Hint"
        Right _ -> expectationFailure "expected error for extra fields"

    it "reports both missing and extra fields together" $ do
      let stmt = mkStatementNamed @SimpleParams @()
            "SELECT 1 WHERE name = $1"
            [25]
            []
            ["name"]  -- SQL expects "name" but record has "userId"
            "test.sql"
      result <- try @ErrorCall $ evaluate (stmtEncode stmt (SimpleParams 1))
      case result of
        Left (ErrorCall msg) -> do
          msg `shouldContain` "Missing fields"
          msg `shouldContain` ":name"
          msg `shouldContain` "Extra fields"
          msg `shouldContain` "userId"
        Right _ -> expectationFailure "expected error for mismatched fields"

-- Use hspec's shouldContain (works on [Char] = String)

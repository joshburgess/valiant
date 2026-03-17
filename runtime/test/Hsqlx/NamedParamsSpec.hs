{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hsqlx.NamedParamsSpec (spec) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Hsqlx.NamedParams
import Hsqlx.Statement (Statement (..))
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

spec :: Spec
spec = do
  describe "ToNamedParams (Generic)" $ do
    it "encodes a single-field record" $ do
      let pairs = toNamedParamList (SimpleParams 42)
      length pairs `shouldBe` 1
      case pairs of
        [(name, val)] -> do
          name `shouldBe` "userId"
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

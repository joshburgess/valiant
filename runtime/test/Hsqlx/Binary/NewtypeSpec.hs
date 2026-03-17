{-# LANGUAGE DerivingVia #-}

module Hsqlx.Binary.NewtypeSpec (spec) where

import Data.Int (Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Vector qualified as V
import Hsqlx.Binary.Decode ()
import Hsqlx.Binary.Encode ()
import Hsqlx.FromRow (DecodeColumn (..), FromRow (..))
import Hsqlx.ToParams (EncodeField (..), ToParams (..))
import PgWire.Binary.Types (PgDecode (..), PgEncode (..))
import PgWire.Protocol.Oid (oidInt4, oidInt8, oidText)
import Test.Hspec

-- | A newtype that should inherit PgEncode/PgDecode via GeneralizedNewtypeDeriving.
newtype UserId = UserId Int32
  deriving stock (Show, Eq)
  deriving newtype (PgEncode, PgDecode)

-- | Another newtype wrapping Text.
newtype Email = Email Text
  deriving stock (Show, Eq)
  deriving newtype (PgEncode, PgDecode)

-- | A newtype wrapping Int64.
newtype PostId = PostId Int64
  deriving stock (Show, Eq)
  deriving newtype (PgEncode, PgDecode)

-- | A newtype using DerivingVia instead of GeneralizedNewtypeDeriving.
newtype OrgId = OrgId Int32
  deriving stock (Show, Eq)
  deriving (PgEncode, PgDecode) via Int32

spec :: Spec
spec = do
  describe "GeneralizedNewtypeDeriving for PgEncode" $ do
    it "UserId encodes identically to Int32" $ do
      pgEncode (UserId 42) `shouldBe` pgEncode (42 :: Int32)

    it "UserId has the same OID as Int32" $ do
      pgOid (Proxy @UserId) `shouldBe` oidInt4

    it "Email encodes identically to Text" $ do
      pgEncode (Email "alice@example.com") `shouldBe` pgEncode ("alice@example.com" :: Text)

    it "Email has the same OID as Text" $ do
      pgOid (Proxy @Email) `shouldBe` oidText

    it "PostId encodes identically to Int64" $ do
      pgEncode (PostId 999) `shouldBe` pgEncode (999 :: Int64)

    it "PostId has the same OID as Int64" $ do
      pgOid (Proxy @PostId) `shouldBe` oidInt8

  describe "GeneralizedNewtypeDeriving for PgDecode" $ do
    it "decodes UserId from Int32 bytes" $ do
      let bs = pgEncode (42 :: Int32)
      pgDecode bs `shouldBe` Right (UserId 42)

    it "decodes Email from Text bytes" $ do
      let bs = pgEncode ("test@example.com" :: Text)
      pgDecode bs `shouldBe` Right (Email "test@example.com")

    it "decodes PostId from Int64 bytes" $ do
      let bs = pgEncode (123 :: Int64)
      pgDecode bs `shouldBe` Right (PostId 123)

  describe "DerivingVia for PgEncode/PgDecode" $ do
    it "OrgId encodes identically to Int32" $ do
      pgEncode (OrgId 7) `shouldBe` pgEncode (7 :: Int32)

    it "OrgId has the same OID as Int32" $ do
      pgOid (Proxy @OrgId) `shouldBe` oidInt4

    it "OrgId round-trips" $ do
      let bs = pgEncode (OrgId 7)
      pgDecode bs `shouldBe` Right (OrgId 7)

  describe "Newtype round-trip" $ do
    it "UserId encode/decode is identity" $ do
      let uid = UserId 12345
      pgDecode (pgEncode uid) `shouldBe` Right uid

    it "Email encode/decode is identity" $ do
      let email = Email "hello@world.com"
      pgDecode (pgEncode email) `shouldBe` Right email

  describe "Newtypes work with EncodeField" $ do
    it "EncodeField encodes UserId" $ do
      encodeField (UserId 42) `shouldBe` Just (pgEncode (42 :: Int32))

    it "EncodeField encodes Maybe UserId (Just)" $ do
      encodeField (Just (UserId 42)) `shouldBe` Just (pgEncode (42 :: Int32))

    it "EncodeField encodes Maybe UserId (Nothing)" $ do
      encodeField (Nothing @UserId) `shouldBe` Nothing

  describe "Newtypes work with ToParams" $ do
    it "single UserId as ToParams" $ do
      let params = toParams (UserId 42)
      V.length params `shouldBe` 1
      V.head params `shouldBe` Just (pgEncode (42 :: Int32))

    it "tuple of newtypes as ToParams" $ do
      let params = toParams (UserId 1, Email "a@b.com")
      V.length params `shouldBe` 2

  describe "Newtypes work with DecodeColumn" $ do
    it "decodes UserId from a row" $ do
      let row = V.singleton (Just (pgEncode (42 :: Int32)))
      decodeColumn row 0 `shouldBe` Right (UserId 42)

    it "decodes Maybe UserId from NULL" $ do
      let row = V.singleton Nothing
      decodeColumn row 0 `shouldBe` Right (Nothing @UserId)

    it "decodes Maybe UserId from non-NULL" $ do
      let row = V.singleton (Just (pgEncode (42 :: Int32)))
      decodeColumn row 0 `shouldBe` Right (Just (UserId 42))

  describe "Newtypes work with FromRow" $ do
    it "decodes single UserId via FromRow" $ do
      let row = V.singleton (Just (pgEncode (42 :: Int32)))
      fromRow row `shouldBe` Right (UserId 42)

    it "decodes tuple with newtypes via FromRow" $ do
      let row = V.fromList [Just (pgEncode (1 :: Int32)), Just (pgEncode ("alice" :: Text))]
      fromRow row `shouldBe` Right (UserId 1, Email "alice")

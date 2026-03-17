{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module PgWire.Connection.ConfigSpec (spec) where

import PgWire.Connection.Config
import Test.Hspec

spec :: Spec
spec = do
  describe "parseConnString (URI format)" $ do
    it "parses basic postgres:// URL" $ do
      let Right cfg = parseConnString "postgres://user:pass@localhost:5432/mydb"
      ccHost cfg `shouldBe` "localhost"
      ccPort cfg `shouldBe` 5432
      ccUser cfg `shouldBe` "user"
      ccPassword cfg `shouldBe` "pass"
      ccDatabase cfg `shouldBe` "mydb"

    it "parses postgresql:// scheme" $ do
      let Right cfg = parseConnString "postgresql://u:p@host:1234/db"
      ccHost cfg `shouldBe` "host"
      ccPort cfg `shouldBe` 1234

    it "defaults port to 5432" $ do
      let Right cfg = parseConnString "postgres://u:p@myhost/db"
      ccPort cfg `shouldBe` 5432

    it "parses sslmode=require" $ do
      let Right cfg = parseConnString "postgres://u:p@h:5432/db?sslmode=require"
      ccTls cfg `shouldBe` TlsRequire

    it "parses connect_timeout from query params" $ do
      let Right cfg = parseConnString "postgres://u:p@h:5432/db?connect_timeout=30"
      ccConnectTimeout cfg `shouldBe` 30

    it "defaults connect timeout to 10" $ do
      let Right cfg = parseConnString "postgres://u:p@h:5432/db"
      ccConnectTimeout cfg `shouldBe` 10

    it "defaults query timeout to 0 (no timeout)" $ do
      let Right cfg = parseConnString "postgres://u:p@h:5432/db"
      ccQueryTimeout cfg `shouldBe` 0

  describe "parseConnString (key=value format)" $ do
    it "parses key=value pairs" $ do
      let Right cfg = parseConnString "host=myhost port=9999 user=alice password=secret dbname=testdb"
      ccHost cfg `shouldBe` "myhost"
      ccPort cfg `shouldBe` 9999
      ccUser cfg `shouldBe` "alice"
      ccPassword cfg `shouldBe` "secret"
      ccDatabase cfg `shouldBe` "testdb"

    it "defaults missing fields" $ do
      let Right cfg = parseConnString "host=myhost dbname=db"
      ccPort cfg `shouldBe` 5432
      ccUser cfg `shouldBe` ""

    it "parses connect_timeout" $ do
      let Right cfg = parseConnString "host=h connect_timeout=5"
      ccConnectTimeout cfg `shouldBe` 5

    it "parses sslmode" $ do
      let Right cfg = parseConnString "host=h sslmode=prefer"
      ccTls cfg `shouldBe` TlsPrefer

  describe "defaultConnConfig" $ do
    it "has sensible defaults" $ do
      ccHost defaultConnConfig `shouldBe` "localhost"
      ccPort defaultConnConfig `shouldBe` 5432
      ccTls defaultConnConfig `shouldBe` TlsDisable
      ccConnectTimeout defaultConnConfig `shouldBe` 10
      ccQueryTimeout defaultConnConfig `shouldBe` 0

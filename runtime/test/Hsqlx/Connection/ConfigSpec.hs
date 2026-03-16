module Hsqlx.Connection.ConfigSpec (spec) where

import Hsqlx.Connection.Config
import Test.Hspec

spec :: Spec
spec = do
  describe "parseConnString (URI format)" $ do
    it "parses a full postgres:// URI" $ do
      let Right cfg = parseConnString "postgres://alice:secret@db.example.com:5433/mydb"
      ccHost cfg `shouldBe` "db.example.com"
      ccPort cfg `shouldBe` 5433
      ccUser cfg `shouldBe` "alice"
      ccPassword cfg `shouldBe` "secret"
      ccDatabase cfg `shouldBe` "mydb"

    it "parses postgresql:// scheme" $ do
      let Right cfg = parseConnString "postgresql://user:pass@localhost:5432/testdb"
      ccHost cfg `shouldBe` "localhost"
      ccPort cfg `shouldBe` 5432
      ccUser cfg `shouldBe` "user"
      ccPassword cfg `shouldBe` "pass"
      ccDatabase cfg `shouldBe` "testdb"

    it "uses default port 5432 when port is omitted" $ do
      let Right cfg = parseConnString "postgres://user:pass@localhost/mydb"
      ccPort cfg `shouldBe` 5432
      ccHost cfg `shouldBe` "localhost"

    it "handles missing password" $ do
      let Right cfg = parseConnString "postgres://user@localhost:5432/mydb"
      ccUser cfg `shouldBe` "user"
      ccPassword cfg `shouldBe` ""

    it "handles missing user and password" $ do
      let Right cfg = parseConnString "postgres://localhost:5432/mydb"
      ccUser cfg `shouldBe` ""
      ccPassword cfg `shouldBe` ""

    it "parses sslmode=require from query string" $ do
      let Right cfg = parseConnString "postgres://user:pass@localhost/mydb?sslmode=require"
      ccTls cfg `shouldBe` TlsRequire

    it "parses sslmode=prefer from query string" $ do
      let Right cfg = parseConnString "postgres://user:pass@localhost/mydb?sslmode=prefer"
      ccTls cfg `shouldBe` TlsPrefer

    it "parses sslmode=disable from query string" $ do
      let Right cfg = parseConnString "postgres://user:pass@localhost/mydb?sslmode=disable"
      ccTls cfg `shouldBe` TlsDisable

    it "defaults TLS to disable when no sslmode" $ do
      let Right cfg = parseConnString "postgres://user:pass@localhost/mydb"
      ccTls cfg `shouldBe` TlsDisable

    it "defaults app name to hsqlx" $ do
      let Right cfg = parseConnString "postgres://user:pass@localhost/mydb"
      ccAppName cfg `shouldBe` "hsqlx"

  describe "parseConnString (key=value format)" $ do
    it "parses a full key=value connection string" $ do
      let Right cfg = parseConnString "host=db.example.com port=5433 dbname=mydb user=alice password=secret"
      ccHost cfg `shouldBe` "db.example.com"
      ccPort cfg `shouldBe` 5433
      ccDatabase cfg `shouldBe` "mydb"
      ccUser cfg `shouldBe` "alice"
      ccPassword cfg `shouldBe` "secret"

    it "uses defaults for missing keys" $ do
      let Right cfg = parseConnString "host=myhost dbname=mydb"
      ccHost cfg `shouldBe` "myhost"
      ccPort cfg `shouldBe` 5432
      ccUser cfg `shouldBe` ""
      ccPassword cfg `shouldBe` ""
      ccTls cfg `shouldBe` TlsDisable

    it "defaults host to localhost when omitted" $ do
      let Right cfg = parseConnString "dbname=mydb user=alice"
      ccHost cfg `shouldBe` "localhost"

    it "parses sslmode=require" $ do
      let Right cfg = parseConnString "host=localhost dbname=mydb sslmode=require"
      ccTls cfg `shouldBe` TlsRequire

    it "accepts 'database' as alias for 'dbname'" $ do
      let Right cfg = parseConnString "host=localhost database=mydb"
      ccDatabase cfg `shouldBe` "mydb"

    it "parses application_name" $ do
      let Right cfg = parseConnString "host=localhost dbname=mydb application_name=myapp"
      ccAppName cfg `shouldBe` "myapp"

    it "defaults application_name to hsqlx" $ do
      let Right cfg = parseConnString "host=localhost dbname=mydb"
      ccAppName cfg `shouldBe` "hsqlx"

  describe "defaultConnConfig" $ do
    it "has sensible defaults" $ do
      ccHost defaultConnConfig `shouldBe` "localhost"
      ccPort defaultConnConfig `shouldBe` 5432
      ccDatabase defaultConnConfig `shouldBe` ""
      ccUser defaultConnConfig `shouldBe` ""
      ccPassword defaultConnConfig `shouldBe` ""
      ccTls defaultConnConfig `shouldBe` TlsDisable
      ccAppName defaultConnConfig `shouldBe` "hsqlx"

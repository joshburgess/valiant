{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module PgWire.Connection.FeaturesSpec (spec) where

import PgWire.Connection.Config
import Test.Hspec

spec :: Spec
spec = do
  describe "parseHosts" $ do
    it "parses single host" $
      parseHosts "localhost" `shouldBe` ["localhost"]

    it "parses multiple hosts" $
      parseHosts "host1,host2,host3" `shouldBe` ["host1", "host2", "host3"]

    it "returns localhost for empty string" $
      parseHosts "" `shouldBe` ["localhost"]

    it "handles two hosts" $
      parseHosts "primary,replica" `shouldBe` ["primary", "replica"]

  describe "TlsMode parsing" $ do
    it "parses verify-ca from URI" $ do
      let Right cfg = parseConnString "postgres://u:p@h:5432/db?sslmode=verify-ca"
      ccTls cfg `shouldBe` TlsVerifyCa

    it "parses verify-full from URI" $ do
      let Right cfg = parseConnString "postgres://u:p@h:5432/db?sslmode=verify-full"
      ccTls cfg `shouldBe` TlsVerifyFull

    it "parses verify-ca from key-value" $ do
      let Right cfg = parseConnString "host=h sslmode=verify-ca"
      ccTls cfg `shouldBe` TlsVerifyCa

    it "parses verify-full from key-value" $ do
      let Right cfg = parseConnString "host=h sslmode=verify-full"
      ccTls cfg `shouldBe` TlsVerifyFull

  describe "SSL cert params" $ do
    it "parses sslcert from URI" $ do
      let Right cfg = parseConnString "postgres://u:p@h:5432/db?sslcert=/path/cert.pem"
      ccSslCert cfg `shouldBe` Just "/path/cert.pem"

    it "parses sslkey from key-value" $ do
      let Right cfg = parseConnString "host=h sslkey=/path/key.pem"
      ccSslKey cfg `shouldBe` Just "/path/key.pem"

    it "parses sslrootcert" $ do
      let Right cfg = parseConnString "host=h sslrootcert=/path/ca.pem"
      ccSslRootCert cfg `shouldBe` Just "/path/ca.pem"

    it "defaults to Nothing" $ do
      let Right cfg = parseConnString "host=h"
      ccSslCert cfg `shouldBe` Nothing
      ccSslKey cfg `shouldBe` Nothing
      ccSslRootCert cfg `shouldBe` Nothing

  describe "keepalive config" $ do
    it "parses keepalives=0 as False" $ do
      let Right cfg = parseConnString "host=h keepalives=0"
      ccKeepalives cfg `shouldBe` False

    it "defaults keepalives to True" $ do
      let Right cfg = parseConnString "host=h"
      ccKeepalives cfg `shouldBe` True

    it "parses keepalives_idle" $ do
      let Right cfg = parseConnString "host=h keepalives_idle=60"
      ccKeepalivesIdle cfg `shouldBe` 60

    it "parses keepalives_interval" $ do
      let Right cfg = parseConnString "host=h keepalives_interval=10"
      ccKeepalivesInterval cfg `shouldBe` 10

    it "parses keepalives_count" $ do
      let Right cfg = parseConnString "host=h keepalives_count=5"
      ccKeepalivesCount cfg `shouldBe` 5

  describe "target_session_attrs" $ do
    it "parses read-write from URI" $ do
      let Right cfg = parseConnString "postgres://u:p@h:5432/db?target_session_attrs=read-write"
      ccTargetSessionAttrs cfg `shouldBe` SessionReadWrite

    it "parses primary from key-value" $ do
      let Right cfg = parseConnString "host=h target_session_attrs=primary"
      ccTargetSessionAttrs cfg `shouldBe` SessionPrimary

    it "parses standby" $ do
      let Right cfg = parseConnString "host=h target_session_attrs=standby"
      ccTargetSessionAttrs cfg `shouldBe` SessionStandby

    it "parses prefer-standby" $ do
      let Right cfg = parseConnString "host=h target_session_attrs=prefer-standby"
      ccTargetSessionAttrs cfg `shouldBe` SessionPreferStandby

    it "parses read-only" $ do
      let Right cfg = parseConnString "host=h target_session_attrs=read-only"
      ccTargetSessionAttrs cfg `shouldBe` SessionReadOnly

    it "defaults to any" $ do
      let Right cfg = parseConnString "host=h"
      ccTargetSessionAttrs cfg `shouldBe` SessionAny

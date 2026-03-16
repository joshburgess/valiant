module PgWire.Protocol.OidSpec (spec) where

import PgWire.Protocol.Oid
import Test.Hspec

spec :: Spec
spec = do
  describe "OID constants" $ do
    it "oidBool = 16" $ unOid oidBool `shouldBe` 16
    it "oidInt4 = 23" $ unOid oidInt4 `shouldBe` 23
    it "oidInt8 = 20" $ unOid oidInt8 `shouldBe` 20
    it "oidText = 25" $ unOid oidText `shouldBe` 25
    it "oidTimestamptz = 1184" $ unOid oidTimestamptz `shouldBe` 1184
    it "oidUuid = 2950" $ unOid oidUuid `shouldBe` 2950
    it "oidJsonb = 3802" $ unOid oidJsonb `shouldBe` 3802
    it "oidInterval = 1186" $ unOid oidInterval `shouldBe` 1186

  describe "array OID functions" $ do
    it "isArrayOid recognises int4[]" $ isArrayOid (Oid 1007) `shouldBe` True
    it "isArrayOid rejects int4" $ isArrayOid (Oid 23) `shouldBe` False
    it "arrayElementOid maps int4[] to int4" $
      arrayElementOid (Oid 1007) `shouldBe` Just oidInt4
    it "arrayElementOid maps text[] to text" $
      arrayElementOid (Oid 1009) `shouldBe` Just oidText
    it "arrayElementOid returns Nothing for scalar" $
      arrayElementOid (Oid 23) `shouldBe` Nothing

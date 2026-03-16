module PgWire.Protocol.OidSpec (spec) where

import PgWire.Protocol.Oid
import Test.Hspec

spec :: Spec
spec = do
  describe "scalar OID constants" $ do
    it "oidBool = 16" $ unOid oidBool `shouldBe` 16
    it "oidBytea = 17" $ unOid oidBytea `shouldBe` 17
    it "oidInt2 = 21" $ unOid oidInt2 `shouldBe` 21
    it "oidInt4 = 23" $ unOid oidInt4 `shouldBe` 23
    it "oidInt8 = 20" $ unOid oidInt8 `shouldBe` 20
    it "oidText = 25" $ unOid oidText `shouldBe` 25
    it "oidJson = 114" $ unOid oidJson `shouldBe` 114
    it "oidFloat4 = 700" $ unOid oidFloat4 `shouldBe` 700
    it "oidFloat8 = 701" $ unOid oidFloat8 `shouldBe` 701
    it "oidVarchar = 1043" $ unOid oidVarchar `shouldBe` 1043
    it "oidDate = 1082" $ unOid oidDate `shouldBe` 1082
    it "oidTime = 1083" $ unOid oidTime `shouldBe` 1083
    it "oidTimestamp = 1114" $ unOid oidTimestamp `shouldBe` 1114
    it "oidTimestamptz = 1184" $ unOid oidTimestamptz `shouldBe` 1184
    it "oidNumeric = 1700" $ unOid oidNumeric `shouldBe` 1700
    it "oidUuid = 2950" $ unOid oidUuid `shouldBe` 2950
    it "oidJsonb = 3802" $ unOid oidJsonb `shouldBe` 3802
    it "oidInterval = 1186" $ unOid oidInterval `shouldBe` 1186

  describe "array OID constants" $ do
    it "oidBoolArray = 1000" $ unOid oidBoolArray `shouldBe` 1000
    it "oidByteaArray = 1001" $ unOid oidByteaArray `shouldBe` 1001
    it "oidInt2Array = 1005" $ unOid oidInt2Array `shouldBe` 1005
    it "oidInt4Array = 1007" $ unOid oidInt4Array `shouldBe` 1007
    it "oidInt8Array = 1016" $ unOid oidInt8Array `shouldBe` 1016
    it "oidTextArray = 1009" $ unOid oidTextArray `shouldBe` 1009
    it "oidVarcharArray = 1015" $ unOid oidVarcharArray `shouldBe` 1015
    it "oidFloat4Array = 1021" $ unOid oidFloat4Array `shouldBe` 1021
    it "oidFloat8Array = 1022" $ unOid oidFloat8Array `shouldBe` 1022
    it "oidTimestampArray = 1115" $ unOid oidTimestampArray `shouldBe` 1115
    it "oidTimestamptzArray = 1185" $ unOid oidTimestamptzArray `shouldBe` 1185
    it "oidDateArray = 1182" $ unOid oidDateArray `shouldBe` 1182
    it "oidTimeArray = 1183" $ unOid oidTimeArray `shouldBe` 1183
    it "oidUuidArray = 2951" $ unOid oidUuidArray `shouldBe` 2951
    it "oidJsonArray = 199" $ unOid oidJsonArray `shouldBe` 199
    it "oidJsonbArray = 3807" $ unOid oidJsonbArray `shouldBe` 3807

  describe "isArrayOid" $ do
    it "True for all 16 array OIDs" $ do
      let arrayOids = [1000, 1001, 1005, 1007, 1009, 1015, 1016,
                        1021, 1022, 1115, 1182, 1183, 1185, 2951, 199, 3807]
      mapM_ (\o -> isArrayOid (Oid o) `shouldBe` True) arrayOids

    it "False for all scalar OIDs" $ do
      let scalarOids = [16, 17, 20, 21, 23, 25, 114, 700, 701,
                         1043, 1082, 1083, 1114, 1184, 1186, 1700, 2950, 3802]
      mapM_ (\o -> isArrayOid (Oid o) `shouldBe` False) scalarOids

    it "False for unknown OID" $
      isArrayOid (Oid 99999) `shouldBe` False

  describe "arrayElementOid" $ do
    it "bool[] → bool" $ arrayElementOid oidBoolArray `shouldBe` Just oidBool
    it "bytea[] → bytea" $ arrayElementOid oidByteaArray `shouldBe` Just oidBytea
    it "int2[] → int2" $ arrayElementOid oidInt2Array `shouldBe` Just oidInt2
    it "int4[] → int4" $ arrayElementOid oidInt4Array `shouldBe` Just oidInt4
    it "int8[] → int8" $ arrayElementOid oidInt8Array `shouldBe` Just oidInt8
    it "text[] → text" $ arrayElementOid oidTextArray `shouldBe` Just oidText
    it "varchar[] → varchar" $ arrayElementOid oidVarcharArray `shouldBe` Just oidVarchar
    it "float4[] → float4" $ arrayElementOid oidFloat4Array `shouldBe` Just oidFloat4
    it "float8[] → float8" $ arrayElementOid oidFloat8Array `shouldBe` Just oidFloat8
    it "timestamp[] → timestamp" $ arrayElementOid oidTimestampArray `shouldBe` Just oidTimestamp
    it "timestamptz[] → timestamptz" $ arrayElementOid oidTimestamptzArray `shouldBe` Just oidTimestamptz
    it "date[] → date" $ arrayElementOid oidDateArray `shouldBe` Just oidDate
    it "time[] → time" $ arrayElementOid oidTimeArray `shouldBe` Just oidTime
    it "uuid[] → uuid" $ arrayElementOid oidUuidArray `shouldBe` Just oidUuid
    it "json[] → json" $ arrayElementOid oidJsonArray `shouldBe` Just oidJson
    it "jsonb[] → jsonb" $ arrayElementOid oidJsonbArray `shouldBe` Just oidJsonb
    it "scalar → Nothing" $ arrayElementOid oidInt4 `shouldBe` Nothing
    it "unknown → Nothing" $ arrayElementOid (Oid 99999) `shouldBe` Nothing

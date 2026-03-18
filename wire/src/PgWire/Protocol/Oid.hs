module PgWire.Protocol.Oid
  ( Oid (..)
  , oidBool
  , oidBytea
  , oidInt8
  , oidInt2
  , oidInt4
  , oidText
  , oidJson
  , oidFloat4
  , oidFloat8
  , oidVarchar
  , oidDate
  , oidTime
  , oidTimestamp
  , oidTimestamptz
  , oidNumeric
  , oidUuid
  , oidJsonb
  , oidInterval
  , oidInet
  , oidCidr
  , oidMacAddr
  , oidMacAddr8
    -- * Array OIDs
  , oidBoolArray
  , oidByteaArray
  , oidInt2Array
  , oidInt4Array
  , oidInt8Array
  , oidTextArray
  , oidVarcharArray
  , oidFloat4Array
  , oidFloat8Array
  , oidTimestampArray
  , oidTimestamptzArray
  , oidDateArray
  , oidTimeArray
  , oidUuidArray
  , oidJsonArray
  , oidJsonbArray
  , isArrayOid
  , arrayElementOid
  ) where

import Data.Word (Word32)

-- | A PostgreSQL type OID.
newtype Oid = Oid {unOid :: Word32}
  deriving stock (Show, Eq, Ord)

oidBool, oidBytea, oidInt8, oidInt2, oidInt4 :: Oid
oidBool = Oid 16
oidBytea = Oid 17
oidInt8 = Oid 20
oidInt2 = Oid 21
oidInt4 = Oid 23

oidText, oidJson, oidFloat4, oidFloat8 :: Oid
oidText = Oid 25
oidJson = Oid 114
oidFloat4 = Oid 700
oidFloat8 = Oid 701

oidVarchar, oidDate, oidTime, oidTimestamp, oidTimestamptz :: Oid
oidVarchar = Oid 1043
oidDate = Oid 1082
oidTime = Oid 1083
oidTimestamp = Oid 1114
oidTimestamptz = Oid 1184

oidNumeric, oidUuid, oidJsonb, oidInterval, oidInet, oidCidr, oidMacAddr, oidMacAddr8 :: Oid
oidNumeric = Oid 1700
oidUuid = Oid 2950
oidJsonb = Oid 3802
oidInterval = Oid 1186
oidInet = Oid 869
oidCidr = Oid 650
oidMacAddr = Oid 829
oidMacAddr8 = Oid 774

-- Array OIDs ----------------------------------------------------------------

oidBoolArray, oidByteaArray, oidInt2Array, oidInt4Array, oidInt8Array :: Oid
oidBoolArray = Oid 1000
oidByteaArray = Oid 1001
oidInt2Array = Oid 1005
oidInt4Array = Oid 1007
oidInt8Array = Oid 1016

oidTextArray, oidVarcharArray :: Oid
oidTextArray = Oid 1009
oidVarcharArray = Oid 1015

oidFloat4Array, oidFloat8Array :: Oid
oidFloat4Array = Oid 1021
oidFloat8Array = Oid 1022

oidTimestampArray, oidTimestamptzArray, oidDateArray, oidTimeArray :: Oid
oidTimestampArray = Oid 1115
oidTimestamptzArray = Oid 1185
oidDateArray = Oid 1182
oidTimeArray = Oid 1183

oidUuidArray, oidJsonArray, oidJsonbArray :: Oid
oidUuidArray = Oid 2951
oidJsonArray = Oid 199
oidJsonbArray = Oid 3807

-- | Check whether an OID is a known array type.
isArrayOid :: Oid -> Bool
isArrayOid oid = case arrayElementOid oid of
  Just _ -> True
  Nothing -> False

-- | For an array OID, return the element OID.
arrayElementOid :: Oid -> Maybe Oid
arrayElementOid (Oid o) = case o of
  1000 -> Just oidBool
  1001 -> Just oidBytea
  1005 -> Just oidInt2
  1007 -> Just oidInt4
  1009 -> Just oidText
  1015 -> Just oidVarchar
  1016 -> Just oidInt8
  1021 -> Just oidFloat4
  1022 -> Just oidFloat8
  1115 -> Just oidTimestamp
  1182 -> Just oidDate
  1183 -> Just oidTime
  1185 -> Just oidTimestamptz
  2951 -> Just oidUuid
  199 -> Just oidJson
  3807 -> Just oidJsonb
  _ -> Nothing

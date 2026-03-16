module Hsqlx.CLI.TypeMap
  ( HaskellType (..)
  , oidToHaskellType
  , oidToTypeName
  , resolveType
  , oidToHaskellTypeWith
  , oidToTypeNameWith
  , isArrayOid
  , arrayElementOid
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Database.PostgreSQL.LibPQ (Oid (..))

-- | A Haskell type with its originating module.
data HaskellType = HaskellType
  { htType :: Text
  , htModule :: Text
  }
  deriving stock (Show, Eq)

-- | Wrap a type in @Maybe@ if nullable.
resolveType :: Bool -> HaskellType -> HaskellType
resolveType nullable ht
  | nullable = ht {htType = "Maybe " <> htType ht}
  | otherwise = ht

-- | Look up the Haskell type for a Postgres OID (built-in types only).
oidToHaskellType :: Oid -> Maybe HaskellType
oidToHaskellType oid = Map.lookup oid oidMap

-- | Look up the Haskell type for a Postgres OID, falling back to a custom map.
oidToHaskellTypeWith :: Map Oid HaskellType -> Oid -> Maybe HaskellType
oidToHaskellTypeWith customs oid =
  case Map.lookup oid oidMap of
    Just ht -> Just ht
    Nothing -> case Map.lookup oid customs of
      Just ht -> Just ht
      Nothing -> arrayHaskellType oid customs

-- | Look up the Postgres type name for an OID.
oidToTypeName :: Oid -> Maybe Text
oidToTypeName oid = Map.lookup oid oidNameMap

-- | Look up the Postgres type name for an OID, falling back to a custom map.
oidToTypeNameWith :: Map Oid Text -> Oid -> Maybe Text
oidToTypeNameWith customs oid =
  Map.lookup oid oidNameMap <> Map.lookup oid customs

-- Array support -------------------------------------------------------------

-- | Check whether an OID is a known array type.
isArrayOid :: Oid -> Bool
isArrayOid oid = Map.member oid arrayOidToElement

-- | For an array OID, return the element OID.
arrayElementOid :: Oid -> Maybe Oid
arrayElementOid oid = Map.lookup oid arrayOidToElement

-- | Derive the Haskell type for an array OID: @Vector <elementType>@.
arrayHaskellType :: Oid -> Map Oid HaskellType -> Maybe HaskellType
arrayHaskellType oid customs = do
  elemOid <- arrayElementOid oid
  elemHt <- case Map.lookup elemOid oidMap of
    Just ht -> Just ht
    Nothing -> Map.lookup elemOid customs
  pure
    HaskellType
      { htType = "Vector " <> htType elemHt
      , htModule = "Data.Vector"
      }

-- Internal mapping tables ------------------------------------------------

oidMap :: Map Oid HaskellType
oidMap =
  Map.fromList
    [ (Oid 16, HaskellType "Bool" "Prelude")
    , (Oid 17, HaskellType "ByteString" "Data.ByteString")
    , (Oid 20, HaskellType "Int64" "Data.Int")
    , (Oid 21, HaskellType "Int16" "Data.Int")
    , (Oid 23, HaskellType "Int32" "Data.Int")
    , (Oid 25, HaskellType "Text" "Data.Text")
    , (Oid 114, HaskellType "Value" "Data.Aeson")
    , (Oid 700, HaskellType "Float" "Prelude")
    , (Oid 701, HaskellType "Double" "Prelude")
    , (Oid 1043, HaskellType "Text" "Data.Text")
    , (Oid 1082, HaskellType "Day" "Data.Time")
    , (Oid 1083, HaskellType "TimeOfDay" "Data.Time")
    , (Oid 1114, HaskellType "LocalTime" "Data.Time")
    , (Oid 1184, HaskellType "UTCTime" "Data.Time")
    , (Oid 1186, HaskellType "PgInterval" "Hsqlx")
    , (Oid 1700, HaskellType "Scientific" "Data.Scientific")
    , (Oid 2950, HaskellType "UUID" "Data.UUID")
    , (Oid 3802, HaskellType "Value" "Data.Aeson")
    ]

oidNameMap :: Map Oid Text
oidNameMap =
  Map.fromList
    [ (Oid 16, "bool")
    , (Oid 17, "bytea")
    , (Oid 20, "int8")
    , (Oid 21, "int2")
    , (Oid 23, "int4")
    , (Oid 25, "text")
    , (Oid 114, "json")
    , (Oid 700, "float4")
    , (Oid 701, "float8")
    , (Oid 1043, "varchar")
    , (Oid 1082, "date")
    , (Oid 1083, "time")
    , (Oid 1114, "timestamp")
    , (Oid 1184, "timestamptz")
    , (Oid 1186, "interval")
    , (Oid 1700, "numeric")
    , (Oid 2950, "uuid")
    , (Oid 3802, "jsonb")
    -- Array types
    , (Oid 1000, "_bool")
    , (Oid 1001, "_bytea")
    , (Oid 1005, "_int2")
    , (Oid 1007, "_int4")
    , (Oid 1009, "_text")
    , (Oid 1015, "_varchar")
    , (Oid 1016, "_int8")
    , (Oid 1021, "_float4")
    , (Oid 1022, "_float8")
    , (Oid 1115, "_timestamp")
    , (Oid 1182, "_date")
    , (Oid 1183, "_time")
    , (Oid 1185, "_timestamptz")
    , (Oid 2951, "_uuid")
    , (Oid 199, "_json")
    , (Oid 3807, "_jsonb")
    ]

-- | Mapping from array OIDs to their element OIDs.
arrayOidToElement :: Map Oid Oid
arrayOidToElement =
  Map.fromList
    [ (Oid 1000, Oid 16) -- bool[]
    , (Oid 1001, Oid 17) -- bytea[]
    , (Oid 1005, Oid 21) -- int2[]
    , (Oid 1007, Oid 23) -- int4[]
    , (Oid 1009, Oid 25) -- text[]
    , (Oid 1015, Oid 1043) -- varchar[]
    , (Oid 1016, Oid 20) -- int8[]
    , (Oid 1021, Oid 700) -- float4[]
    , (Oid 1022, Oid 701) -- float8[]
    , (Oid 1115, Oid 1114) -- timestamp[]
    , (Oid 1182, Oid 1082) -- date[]
    , (Oid 1183, Oid 1083) -- time[]
    , (Oid 1185, Oid 1184) -- timestamptz[]
    , (Oid 2951, Oid 2950) -- uuid[]
    , (Oid 199, Oid 114) -- json[]
    , (Oid 3807, Oid 3802) -- jsonb[]
    ]

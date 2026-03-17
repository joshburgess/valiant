{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Named parameter support for query execution.
--
-- This module provides 'ToNamedParams', a type class for encoding
-- Haskell records as named query parameters. Field names in the record
-- correspond to @:name@ parameters in the SQL file.
--
-- @
-- data FindParams = FindParams
--   { orgId  :: Int32
--   , role   :: Text
--   , active :: Bool
--   } deriving (Generic, ToNamedParams)
--
-- findByRole :: NamedStatement FindParams [(Int32, Text)]
-- findByRole = mkStatementNamed
--   "SELECT id, name FROM users WHERE org_id = $1 AND role = $2 AND active = $3"
--   [23, 25, 16]
--   ["id", "name"]
--   ["orgId", "role", "active"]  -- maps $1=orgId, $2=role, $3=active
--   "users/find_by_role.sql"
-- @
module Hsqlx.NamedParams
  ( ToNamedParams (..)
  , NamedStatement
  , mkStatementNamed
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics
import Hsqlx.FromRow (FromRow (..))
import Hsqlx.Statement (Statement (..))
import Hsqlx.ToParams (EncodeField (..))
import PgWire.Protocol.Oid (Oid (..))

-- | A statement with named parameters. This is just a type alias —
-- at the wire level it's the same as a positional 'Statement'.
type NamedStatement p r = Statement p r

-- | Encode a record's fields as named parameters.
--
-- The default 'Generic' implementation extracts field names and values,
-- producing an association list of @(fieldName, encodedValue)@.
--
-- Derive with:
--
-- @
-- data MyParams = MyParams { userId :: Int32, role :: Text }
--   deriving stock (Generic)
--   deriving anyclass (ToNamedParams)
-- @
class ToNamedParams a where
  toNamedParamList :: a -> [(Text, Maybe ByteString)]

  default toNamedParamList :: (Generic a, GToNamedParams (Rep a)) => a -> [(Text, Maybe ByteString)]
  toNamedParamList = gToNamedParams . from
  {-# INLINE toNamedParamList #-}

-- | Generic implementation of 'ToNamedParams'.
class GToNamedParams f where
  gToNamedParams :: f p -> [(Text, Maybe ByteString)]

-- Datatype metadata: pass through
instance (GToNamedParams f) => GToNamedParams (D1 c f) where
  gToNamedParams (M1 x) = gToNamedParams x
  {-# INLINE gToNamedParams #-}

-- Constructor metadata: pass through
instance (GToNamedParams f) => GToNamedParams (C1 c f) where
  gToNamedParams (M1 x) = gToNamedParams x
  {-# INLINE gToNamedParams #-}

-- Product: concatenate fields
instance (GToNamedParams f, GToNamedParams g) => GToNamedParams (f :*: g) where
  gToNamedParams (f :*: g) = gToNamedParams f ++ gToNamedParams g
  {-# INLINE gToNamedParams #-}

-- Selector (record field): extract name and encode value
instance (Selector s, EncodeField a) => GToNamedParams (S1 s (Rec0 a)) where
  gToNamedParams m@(M1 (K1 x)) =
    let name = T.pack (selName m)
     in [(name, encodeField x)]
  {-# INLINE gToNamedParams #-}

-- Unit: no fields
instance GToNamedParams U1 where
  gToNamedParams U1 = []
  {-# INLINE gToNamedParams #-}

-- | Construct a 'Statement' that accepts named parameters via a record type.
--
-- The @paramNames@ list defines the mapping: @paramNames !! 0@ is the
-- name for @$1@, @paramNames !! 1@ for @$2@, etc. At runtime,
-- 'ToNamedParams' extracts the record's fields by name and reorders
-- them to match the positional indices.
--
-- If a record field name doesn't match any expected parameter name,
-- or an expected parameter has no matching field, a runtime error
-- is thrown with a clear message.
mkStatementNamed
  :: (ToNamedParams p, FromRow r)
  => String
  -- ^ SQL text (with positional $1, $2, ... — already preprocessed)
  -> [Int]
  -- ^ Parameter OIDs
  -> [String]
  -- ^ Result column names
  -> [String]
  -- ^ Parameter names in positional order ($1=first, $2=second, ...)
  -> String
  -- ^ Source .sql file path
  -> Statement p r
mkStatementNamed sqlStr oids colNames paramNames path =
  Statement
    { stmtSQL = BS8.pack sqlStr
    , stmtFile = path
    , stmtParamOids = V.fromList (map (Oid . fromIntegral) oids)
    , stmtEncode = encodeNamed (map T.pack paramNames)
    , stmtDecode = fromRow
    , stmtColumns = V.fromList (map BS8.pack colNames)
    }

-- | Build the encoder that reorders named params to positional order.
-- Validates at first use that all SQL param names have matching record
-- fields and vice versa, with clear error messages on mismatch.
encodeNamed :: (ToNamedParams p) => [Text] -> p -> Vector (Maybe ByteString)
encodeNamed paramNames p =
  let pairs = toNamedParamList p
      pairMap = Map.fromList pairs
      expectedNames = Set.fromList paramNames
      actualNames = Set.fromList (map fst pairs)
      missing = Set.toList (Set.difference expectedNames actualNames)
      extra = Set.toList (Set.difference actualNames expectedNames)
      n = length paramNames
   in case (missing, extra) of
        ([], []) ->
          V.fromListN n (map (\name -> Map.findWithDefault Nothing name pairMap) paramNames)
        _ ->
          error $ unlines $ filter (not . null) $ concat
            [ [ "hsqlx: named parameter mismatch"
              , ""
              , "  SQL parameters:   " <> showNames paramNames
              , "  Record fields:    " <> showNames (map fst pairs)
              ]
            , if null missing then []
              else [ ""
                   , "  Missing fields (SQL expects these but the record doesn't have them):"
                   , "    " <> intercalate ", " (map (\n' -> ":" <> T.unpack n') missing)
                   ]
            , if null extra then []
              else [ ""
                   , "  Extra fields (record has these but the SQL doesn't use them):"
                   , "    " <> intercalate ", " (map T.unpack extra)
                   , ""
                   , "  Hint: remove unused fields from the record, or add"
                   , "  corresponding :name parameters to the SQL query."
                   ]
            ]
  where
    showNames ns = intercalate ", " (map T.unpack ns)
{-# INLINE encodeNamed #-}

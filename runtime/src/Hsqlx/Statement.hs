{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | The 'Statement' type: a compile-time validated SQL query.
--
-- @p@ is the parameter type, @r@ is the result type. The GHC source plugin
-- verifies that these types match the SQL query's parameters and columns.
--
-- Create statements with 'queryFile' (validated by the plugin) or
-- 'mkStatement' (for manual\/test construction).
module Hsqlx.Statement
  ( Statement (..)
  , query
  , queryFile
  , queryFileAs
  , mkStatement
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Hsqlx.FromRow (FromRow (..))
import PgWire.Protocol.Oid (Oid (..))
import Hsqlx.ToParams (ToParams (..))

-- | A compile-time validated SQL statement.
--
-- @p@ is the parameter type (e.g., @Int32@ or @(Text, Maybe Text)@).
-- @r@ is the result type (e.g., @(Int32, Text)@ or @()@ for commands).
data Statement p r = Statement
  { stmtSQL :: ByteString
  -- ^ The raw SQL text.
  , stmtFile :: FilePath
  -- ^ The source @.sql@ file path (for error messages).
  , stmtParamOids :: Vector Oid
  -- ^ PostgreSQL type OIDs for each parameter.
  , stmtEncode :: p -> Vector (Maybe ByteString)
  -- ^ Encode parameters to binary format.
  , stmtDecode :: Vector (Maybe ByteString) -> Either String r
  -- ^ Decode a result row from binary format.
  , stmtColumns :: Vector ByteString
  -- ^ Column names from the query result.
  }

-- | Constraint that fires a helpful 'TypeError' if the hsqlx plugin is
-- not loaded. The plugin removes this constraint during the parse-phase
-- rewrite (by replacing @queryFile@ with @mkStatement@). If the plugin
-- isn't loaded, GHC tries to solve this constraint and hits the
-- 'TypeError' instance.
class HsqlxPluginRequired

instance
  TypeError
    ( 'Text "queryFile/queryFileAs requires the Hsqlx.Plugin GHC source plugin."
      ':$$: 'Text ""
      ':$$: 'Text "Add this to your module:"
      ':$$: 'Text "  {-# OPTIONS_GHC -fplugin=Hsqlx.Plugin #-}"
      ':$$: 'Text ""
      ':$$: 'Text "Or add to your .cabal file:"
      ':$$: 'Text "  ghc-options: -fplugin=Hsqlx.Plugin"
    )
  => HsqlxPluginRequired

-- | Reference a @.sql@ file for compile-time validation.
--
-- The GHC source plugin intercepts this call and rewrites it to
-- 'mkStatement' with embedded SQL, OIDs, column names, and file path.
--
-- Without the plugin, this produces a compile-time type error with
-- instructions on how to enable the plugin.
queryFile :: HsqlxPluginRequired => FilePath -> Statement p r
queryFile _ = error "unreachable: hsqlx plugin not loaded"

-- | Like 'queryFile' but for named result types with 'FromRow'.
queryFileAs :: HsqlxPluginRequired => FilePath -> Statement p r
queryFileAs _ = error "unreachable: hsqlx plugin not loaded"

-- | Inline SQL with compile-time validation.
--
-- Write SQL directly in Haskell source. The GHC plugin intercepts this
-- call, hashes the SQL text, looks up the type metadata in @.hsqlx\/@,
-- and rewrites to 'mkStatement' with the correct types.
--
-- Requires running @hsqlx prepare@ with the same SQL text present in a
-- @.sql@ file or registered via @hsqlx prepare --inline@.
--
-- @
-- findById :: Statement Int32 (Int32, Text, Maybe Text)
-- findById = query "SELECT id, name, email FROM users WHERE id = $1"
-- @
query :: HsqlxPluginRequired => String -> Statement p r
query _ = error "unreachable: hsqlx plugin not loaded"

-- | Construct a 'Statement' from literal data embedded by the plugin.
-- The plugin rewrites @queryFile "path.sql"@ into a call to this function
-- with the SQL text, parameter OIDs, column names, and file path baked in.
-- Type class constraints resolve the encoder and decoder at compile time.
mkStatement
  :: (ToParams p, FromRow r)
  => String
  -> [Int]
  -> [String]
  -> String
  -> Statement p r
mkStatement sqlStr oids colNames path =
  Statement
    { stmtSQL = BS8.pack sqlStr
    , stmtFile = path
    , stmtParamOids = V.fromList (map (Oid . fromIntegral) oids)
    , stmtEncode = toParams
    , stmtDecode = fromRow
    , stmtColumns = V.fromList (map BS8.pack colNames)
    }

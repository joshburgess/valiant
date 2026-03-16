-- | The 'Statement' type: a compile-time validated SQL query.
--
-- @p@ is the parameter type, @r@ is the result type. The GHC source plugin
-- verifies that these types match the SQL query's parameters and columns.
--
-- Create statements with 'queryFile' (validated by the plugin) or
-- 'mkStatement' (for manual\/test construction).
module Hsqlx.Statement
  ( Statement (..)
  , queryFile
  , queryFileAs
  , mkStatement
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import Hsqlx.FromRow (FromRow (..))
import Hsqlx.Protocol.Oid (Oid (..))
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

-- | Reference a @.sql@ file for compile-time validation.
-- The GHC source plugin intercepts this call and rewrites it.
-- Without the plugin, this produces a type error.
queryFile :: FilePath -> Statement p r
queryFile path =
  error $
    "hsqlx: queryFile "
      <> show path
      <> " was not rewritten by the Hsqlx.Plugin. "
      <> "Add {-# OPTIONS_GHC -fplugin=Hsqlx.Plugin #-} to your module."

-- | Like 'queryFile' but for named result types with 'FromRow'.
queryFileAs :: FilePath -> Statement p r
queryFileAs path =
  error $
    "hsqlx: queryFileAs "
      <> show path
      <> " was not rewritten by the Hsqlx.Plugin. "
      <> "Add {-# OPTIONS_GHC -fplugin=Hsqlx.Plugin #-} to your module."

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

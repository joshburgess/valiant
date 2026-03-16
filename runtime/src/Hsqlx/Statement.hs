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
-- @p@ is the parameter type, @r@ is the result type.
data Statement p r = Statement
  { stmtSQL :: ByteString
  , stmtFile :: FilePath
  , stmtParamOids :: Vector Oid
  , stmtEncode :: p -> Vector (Maybe ByteString)
  , stmtDecode :: Vector (Maybe ByteString) -> Either String r
  , stmtColumns :: Vector ByteString
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

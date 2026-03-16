-- | Map Haskell type names from cache metadata to GHC Type values.
module Hsqlx.Plugin.TypeMap
  ( resolveParamType
  , resolveResultType
  , typeNameToString
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Hsqlx.Plugin.Cache (CacheColumn (..), CacheParam (..))

-- | Build the expected parameter type description from cache metadata.
-- Returns a human-readable type string for comparison against the user's type.
resolveParamType :: [CacheParam] -> Text
resolveParamType [] = "()"
resolveParamType [p] = cpHaskellType p
resolveParamType ps = "(" <> T.intercalate ", " (map cpHaskellType ps) <> ")"

-- | Build the expected result type description from cache metadata.
resolveResultType :: [CacheColumn] -> Text
resolveResultType [] = "()"
resolveResultType [c] = ccHaskellType c
resolveResultType cs = "(" <> T.intercalate ", " (map ccHaskellType cs) <> ")"

-- | Convert a GHC Type to a simple string representation for comparison.
-- This is a simplified comparison that works at the text level rather than
-- requiring full GHC Type resolution.
typeNameToString :: Text -> Text
typeNameToString = id

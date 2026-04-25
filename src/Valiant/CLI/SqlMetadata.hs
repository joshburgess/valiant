module Valiant.CLI.SqlMetadata
  ( SqlMetadata (..)
  , parseSqlMetadata
  , defaultMetadata
  ) where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T

-- | Optional metadata parsed from @-- valiant:@ comments in SQL files.
data SqlMetadata = SqlMetadata
  { smName :: Maybe Text
  -- ^ Override the generated binding name (@-- valiant:name getUserById@).
  , smResult :: Maybe Text
  -- ^ Use a named result type (@-- valiant:result User@).
  , smSingle :: Bool
  -- ^ Expect exactly one row (@-- valiant:single@). If set, the generated
  -- binding returns @r@ instead of @Maybe r@ or @[r]@.
  }
  deriving stock (Show, Eq)

defaultMetadata :: SqlMetadata
defaultMetadata =
  SqlMetadata
    { smName = Nothing
    , smResult = Nothing
    , smSingle = False
    }

-- | Parse @-- valiant:@ directives from SQL file content.
-- Only lines starting with @-- valiant:@ (with optional leading whitespace)
-- are recognised. The directives must appear before the first SQL keyword.
parseSqlMetadata :: Text -> SqlMetadata
parseSqlMetadata = List.foldl' applyDirective defaultMetadata . extractDirectives

extractDirectives :: Text -> [(Text, Text)]
extractDirectives sql =
  [ parseDirective line
  | line <- T.lines sql
  , isDirective line
  ]

isDirective :: Text -> Bool
isDirective line =
  let stripped = T.stripStart line
   in T.isPrefixOf "-- valiant:" stripped

parseDirective :: Text -> (Text, Text)
parseDirective line =
  let stripped = T.stripStart line
      -- Remove "-- valiant:" prefix
      afterPrefix = T.drop 11 stripped -- length "-- valiant:" == 11
      (key, rest) = T.break (== ' ') (T.strip afterPrefix)
   in (T.toLower key, T.strip rest)

applyDirective :: SqlMetadata -> (Text, Text) -> SqlMetadata
applyDirective meta (key, value) = case key of
  "name" | not (T.null value) -> meta {smName = Just value}
  "result" | not (T.null value) -> meta {smResult = Just value}
  "single" -> meta {smSingle = True}
  _ -> meta

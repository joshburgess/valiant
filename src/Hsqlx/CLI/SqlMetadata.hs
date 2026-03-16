module Hsqlx.CLI.SqlMetadata
  ( SqlMetadata (..)
  , parseSqlMetadata
  , defaultMetadata
  ) where

import Data.Text (Text)
import Data.Text qualified as T

-- | Optional metadata parsed from @-- hsqlx:@ comments in SQL files.
data SqlMetadata = SqlMetadata
  { smName :: Maybe Text
  -- ^ Override the generated binding name (@-- hsqlx:name getUserById@).
  , smResult :: Maybe Text
  -- ^ Use a named result type (@-- hsqlx:result User@).
  , smSingle :: Bool
  -- ^ Expect exactly one row (@-- hsqlx:single@). If set, the generated
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

-- | Parse @-- hsqlx:@ directives from SQL file content.
-- Only lines starting with @-- hsqlx:@ (with optional leading whitespace)
-- are recognised. The directives must appear before the first SQL keyword.
parseSqlMetadata :: Text -> SqlMetadata
parseSqlMetadata = foldl applyDirective defaultMetadata . extractDirectives

extractDirectives :: Text -> [(Text, Text)]
extractDirectives sql =
  [ parseDirective line
  | line <- T.lines sql
  , isDirective line
  ]

isDirective :: Text -> Bool
isDirective line =
  let stripped = T.stripStart line
   in T.isPrefixOf "-- hsqlx:" stripped

parseDirective :: Text -> (Text, Text)
parseDirective line =
  let stripped = T.stripStart line
      -- Remove "-- hsqlx:" prefix
      afterPrefix = T.drop 9 stripped -- length "-- hsqlx:" == 9
      (key, rest) = T.break (== ' ') (T.strip afterPrefix)
   in (T.toLower key, T.strip rest)

applyDirective :: SqlMetadata -> (Text, Text) -> SqlMetadata
applyDirective meta (key, value) = case key of
  "name" | not (T.null value) -> meta {smName = Just value}
  "result" | not (T.null value) -> meta {smResult = Just value}
  "single" -> meta {smSingle = True}
  _ -> meta

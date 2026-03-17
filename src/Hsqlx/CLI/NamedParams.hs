-- | Preprocessing for named parameters in SQL files.
--
-- Converts @:name@ syntax to positional @$N@ parameters:
--
-- @
-- SELECT id, name FROM users WHERE org_id = :orgId AND role = :role
-- @
--
-- becomes:
--
-- @
-- SELECT id, name FROM users WHERE org_id = $1 AND role = $2
-- @
--
-- The name-to-position mapping is returned for storage in the cache.
-- A name that appears multiple times maps to the same @$N@.
--
-- Rules:
--
-- * @::@ (Postgres type cast) is not treated as a named parameter
-- * String literals (@\'...\'@), quoted identifiers (@\"...\"@),
--   line comments (@-- ...@), and block comments (@\/\* ... \*\/@)
--   are skipped
-- * A named parameter is @:@ followed by @[a-zA-Z_][a-zA-Z0-9_]*@
module Hsqlx.CLI.NamedParams
  ( NamedParamMapping
  , preprocessNamedParams
  ) where

import Data.ByteString (ByteString)
import Data.Char (isAlpha, isAlphaNum)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- | Mapping from parameter name to its positional index (1-based).
type NamedParamMapping = [(Text, Int)]

-- | Preprocess a SQL bytestring, replacing @:name@ with @$N@.
-- Returns the rewritten SQL and the name-to-position mapping.
-- If the SQL contains no named parameters, returns the original
-- SQL unchanged with an empty mapping.
preprocessNamedParams :: ByteString -> (ByteString, NamedParamMapping)
preprocessNamedParams sqlBs =
  let sql = TE.decodeUtf8 sqlBs
      (rewritten, mapping) = rewrite sql
   in if Map.null mapping
        then (sqlBs, [])
        else (TE.encodeUtf8 rewritten, Map.toAscList mapping)

-- | Rewrite SQL text, replacing :name tokens with $N.
rewrite :: Text -> (Text, Map Text Int)
rewrite = go mempty Map.empty 1
  where
    go !acc !seen !nextIdx txt
      | T.null txt = (acc, seen)
      | otherwise = case T.uncons txt of
          Nothing -> (acc, seen)
          Just (c, rest)
            -- String literal: skip to closing quote
            | c == '\'' ->
                let (lit, after) = spanStringLit rest
                 in go (acc <> T.singleton c <> lit) seen nextIdx after
            -- Quoted identifier: skip to closing double-quote
            | c == '"' ->
                let (qid, after) = spanQuotedId rest
                 in go (acc <> T.singleton c <> qid) seen nextIdx after
            -- Line comment: skip to end of line
            | c == '-', Just ('-', _) <- T.uncons rest ->
                let (comment, after) = T.break (== '\n') rest
                 in go (acc <> T.singleton c <> comment) seen nextIdx after
            -- Block comment: skip to */
            | c == '/', Just ('*', rest2) <- T.uncons rest ->
                let (comment, after) = spanBlockComment rest2
                 in go (acc <> "/*" <> comment) seen nextIdx after
            -- Postgres cast (::) — not a named param
            | c == ':', Just (':', _) <- T.uncons rest ->
                go (acc <> "::") seen nextIdx (T.drop 1 rest)
            -- Named parameter
            | c == ':', not (T.null rest), isParamStart (T.head rest) ->
                let (name, after) = T.span isParamChar rest
                 in case Map.lookup name seen of
                      Just idx ->
                        go (acc <> "$" <> T.pack (show idx)) seen nextIdx after
                      Nothing ->
                        go (acc <> "$" <> T.pack (show nextIdx)) (Map.insert name nextIdx seen) (nextIdx + 1) after
            -- Anything else: pass through
            | otherwise ->
                go (acc <> T.singleton c) seen nextIdx rest

    isParamStart c = isAlpha c || c == '_'
    isParamChar c = isAlphaNum c || c == '_'

-- | Consume a single-quoted string literal (handling '' escapes).
spanStringLit :: Text -> (Text, Text)
spanStringLit = go mempty
  where
    go !acc txt
      | T.null txt = (acc, txt)
      | otherwise = case T.uncons txt of
          Just ('\'', rest)
            | Just ('\'', rest2) <- T.uncons rest ->
                go (acc <> "''") rest2
            | otherwise ->
                (acc <> "'", rest)
          Just (c, rest) ->
              go (acc <> T.singleton c) rest
          Nothing -> (acc, txt)

-- | Consume a double-quoted identifier (handling "" escapes).
spanQuotedId :: Text -> (Text, Text)
spanQuotedId = go mempty
  where
    go !acc txt
      | T.null txt = (acc, txt)
      | otherwise = case T.uncons txt of
          Just ('"', rest)
            | Just ('"', rest2) <- T.uncons rest ->
                go (acc <> "\"\"") rest2
            | otherwise ->
                (acc <> "\"", rest)
          Just (c, rest) ->
              go (acc <> T.singleton c) rest
          Nothing -> (acc, txt)

-- | Consume a block comment up to and including the closing @*/@.
spanBlockComment :: Text -> (Text, Text)
spanBlockComment = go mempty
  where
    go !acc txt
      | T.null txt = (acc, txt)
      | otherwise = case T.uncons txt of
          Just ('*', rest)
            | Just ('/', rest2) <- T.uncons rest ->
                (acc <> "*/", rest2)
          Just (c, rest) ->
              go (acc <> T.singleton c) rest
          Nothing -> (acc, txt)

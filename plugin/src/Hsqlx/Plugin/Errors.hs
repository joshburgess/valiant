-- | Rich compile-time error messages for hsqlx.
module Hsqlx.Plugin.Errors
  ( errSqlFileNotFound
  , errCacheStaleOrMissing
  , errParamTypeMismatch
  , errParamCountMismatch
  , errResultTypeMismatch
  , errColumnCountMismatch
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Plugins (SDoc, text, vcat, (<+>), empty, hcat)
import GHC.Types.SrcLoc (SrcSpan)
import Hsqlx.Plugin.Cache (CacheColumn (..), CacheParam (..))
import Hsqlx.Plugin.Compat (emitPluginError)
import GHC.Tc.Utils.Monad (TcM)

-- Shorthand for concatenation without spaces
(<<>>) :: SDoc -> SDoc -> SDoc
a <<>> b = hcat [a, b]

-- | HSQLX-001: SQL file not found.
errSqlFileNotFound :: SrcSpan -> FilePath -> [FilePath] -> TcM ()
errSqlFileNotFound srcSpan path suggestions =
  emitPluginError srcSpan $
    vcat
      [ text "[HSQLX-001] SQL file not found"
      , text ""
      , text "  queryFile references a file that doesn't exist:"
      , text "    " <<>> text path
      , text ""
      , suggestBlock suggestions
      , text "  Fix: correct the filename in your queryFile call."
      ]

suggestBlock :: [FilePath] -> SDoc
suggestBlock [] = empty
suggestBlock xs =
  vcat $
    [text "  Did you mean one of these?"]
      ++ [text "    - " <<>> text x | x <- take 3 xs]
      ++ [text ""]

-- | HSQLX-002: Cache stale or missing.
errCacheStaleOrMissing :: SrcSpan -> FilePath -> TcM ()
errCacheStaleOrMissing srcSpan path =
  emitPluginError srcSpan $
    vcat
      [ text "[HSQLX-002] Query not prepared"
      , text ""
      , text "  No cached metadata found for:"
      , text "    " <<>> text path
      , text ""
      , text "  This means either:"
      , text "    1. You haven't run `hsqlx prepare` yet, or"
      , text "    2. The SQL file has changed since the last prepare."
      , text ""
      , text "  Fix: run `hsqlx prepare` then rebuild."
      ]

-- | HSQLX-005: Parameter type mismatch.
errParamTypeMismatch :: SrcSpan -> FilePath -> Int -> Text -> Text -> TcM ()
errParamTypeMismatch srcSpan path paramIdx expected actual =
  emitPluginError srcSpan $
    vcat
      [ text "[HSQLX-005] Parameter type mismatch"
      , text ""
      , text "  Parameter $" <<>> text (show paramIdx)
             <+> text "in" <+> text path
      , text ""
      , text "  Postgres expects:" <+> text (T.unpack expected)
      , text "  Your type:       " <+> text (T.unpack actual)
      , text ""
      , text "  Fix: change the parameter type to" <+> text (T.unpack expected)
      ]

-- | HSQLX-006: Parameter count mismatch.
errParamCountMismatch :: SrcSpan -> FilePath -> Int -> Int -> [CacheParam] -> TcM ()
errParamCountMismatch srcSpan path expected actual params =
  emitPluginError srcSpan $
    vcat $
      [ text "[HSQLX-006] Parameter count mismatch"
      , text ""
      , text "  Your type provides" <+> text (show actual) <+> text "parameter(s)"
      , text "  but the query expects" <+> text (show expected)
      , text ""
      , text "  SQL" <+> text ("(" ++ path ++ ")") <+> text "expects:"
      ]
        ++ [text "    $" <<>> text (show (cpIndex p))
             <+> text "::" <+> text (T.unpack (cpHaskellType p))
           | p <- params]
        ++ [ text ""
           , text "  Fix:" <+> fixHint
           ]
  where
    fixHint
      | expected == 0 = text "use () as the parameter type."
      | expected == 1 = text "use a single type, not a tuple."
      | otherwise = text "use a" <+> text (show expected) <<>> text "-tuple for parameters."

-- | HSQLX-003: Result type mismatch (column type mismatch).
errResultTypeMismatch :: SrcSpan -> FilePath -> [(CacheColumn, Text, Bool)] -> TcM ()
errResultTypeMismatch srcSpan path mismatches =
  emitPluginError srcSpan $
    vcat $
      [ text "[HSQLX-003] Result type mismatch"
      , text ""
      , text "  Your type doesn't match what Postgres returns for" <+> text path
      , text ""
      , text "  Column       PG type         Your type        Match"
      , text "  ──────       ───────         ─────────        ─────"
      ]
        ++ [formatMismatch m | m <- mismatches]
        ++ nullabilityHints
        ++ [ text ""
           , text "  Fix: update your type signature to match the expected types."
           ]
  where
    formatMismatch (col, userType, ok) =
      text "  " <<>> padR 13 (ccName col) <<>> padR 16 (ccPgTypeName col) <<>> padR 17 userType
        <<>> text (if ok then "OK" else "MISMATCH")
    padR n t = text (T.unpack t ++ replicate (max 0 (n - T.length t)) ' ')
    nullabilityHints =
      let hints = concatMap nullHint mismatches
       in if null hints then [] else text "" : hints
    nullHint (col, userType, False)
      | ccNullable col && not (T.isPrefixOf "Maybe" (T.strip userType)) =
          [ text "  Column" <+> text (show (T.unpack (ccName col)))
              <+> text "can be NULL. Wrap in Maybe:" <+> text ("Maybe " ++ T.unpack (T.strip userType))
          , text "  Or force non-null in SQL: SELECT ..." <+> text (T.unpack (ccName col))
              <+> text "AS" <+> text (show (T.unpack (ccName col) ++ "!")) <+> text "..."
          ]
      | not (ccNullable col) && T.isPrefixOf "Maybe" (T.strip userType) =
          [ text "  Column" <+> text (show (T.unpack (ccName col)))
              <+> text "is NOT NULL. Remove the Maybe wrapper."
          ]
    nullHint _ = []

-- | HSQLX-004: Column count mismatch.
errColumnCountMismatch :: SrcSpan -> FilePath -> Int -> Int -> [CacheColumn] -> TcM ()
errColumnCountMismatch srcSpan path expected actual cols =
  emitPluginError srcSpan $
    vcat $
      [ text "[HSQLX-004] Column count mismatch"
      , text ""
      , text "  Your type has" <+> text (show actual) <+> text "field(s)"
      , text "  but the query returns" <+> text (show expected) <+> text "column(s)."
      , text ""
      , text "  SQL" <+> text ("(" ++ path ++ ")") <+> text "returns:"
      ]
        ++ [ text "    " <<>> text (show i) <<>> text "." <+> text (T.unpack (ccName c))
               <+> text "::" <+> text (T.unpack (ccHaskellType c))
           | (i, c) <- zip [(1 :: Int) ..] cols
           ]
        ++ [ text ""
           , text "  Fix: update your result type to have" <+> text (show expected) <+> text "field(s)."
           ]

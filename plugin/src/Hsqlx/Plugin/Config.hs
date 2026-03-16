module Hsqlx.Plugin.Config
  ( PluginConfig (..)
  , defaultConfig
  , parseOptions
  ) where

-- | Plugin configuration parsed from @-fplugin-opt@ flags.
data PluginConfig = PluginConfig
  { pcSqlDir :: FilePath
  , pcCacheDir :: FilePath
  , pcOffline :: Bool
  }
  deriving stock (Show, Eq)

defaultConfig :: PluginConfig
defaultConfig =
  PluginConfig
    { pcSqlDir = "sql"
    , pcCacheDir = ".hsqlx"
    , pcOffline = False
    }

-- | Parse plugin command-line options.
-- Options are @key=value@ strings passed via @-fplugin-opt=Hsqlx.Plugin:key=value@.
parseOptions :: [String] -> PluginConfig
parseOptions = foldl applyOpt defaultConfig
  where
    applyOpt cfg opt = case break (== '=') opt of
      ("sql-dir", '=' : val) -> cfg {pcSqlDir = val}
      ("cache-dir", '=' : val) -> cfg {pcCacheDir = val}
      ("offline", '=' : "true") -> cfg {pcOffline = True}
      ("offline", '=' : "false") -> cfg {pcOffline = False}
      _ -> cfg -- ignore unknown options

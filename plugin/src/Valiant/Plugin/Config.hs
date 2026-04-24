module Valiant.Plugin.Config
  ( PluginConfig (..)
  , defaultConfig
  , parseOptions
  , resolveConfig
  ) where

import Data.List (foldl')
import System.Environment (lookupEnv)

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
    , pcCacheDir = ".valiant"
    , pcOffline = False
    }

-- | Parse plugin command-line options.
-- Options are @key=value@ strings passed via @-fplugin-opt=Valiant.Plugin:key=value@.
parseOptions :: [String] -> PluginConfig
parseOptions = foldl' applyOpt defaultConfig
  where
    applyOpt cfg opt = case break (== '=') opt of
      ("sql-dir", '=' : val) -> cfg {pcSqlDir = val}
      ("cache-dir", '=' : val) -> cfg {pcCacheDir = val}
      ("offline", '=' : "true") -> cfg {pcOffline = True}
      ("offline", '=' : "false") -> cfg {pcOffline = False}
      _ -> cfg -- ignore unknown options

-- | Parse plugin options and resolve environment variable overrides.
-- If @VALIANT_OFFLINE@ is set to @\"true\"@ or @\"1\"@, forces offline mode.
resolveConfig :: [String] -> IO PluginConfig
resolveConfig opts = do
  let cfg = parseOptions opts
  mOffline <- lookupEnv "VALIANT_OFFLINE"
  pure $ case mOffline of
    Just "true" -> cfg {pcOffline = True}
    Just "1" -> cfg {pcOffline = True}
    _ -> cfg

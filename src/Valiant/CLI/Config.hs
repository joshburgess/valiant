module Valiant.CLI.Config
  ( AppEnv (..)
  , resolveEnv
  ) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import System.Environment (lookupEnv)

-- | Resolved application environment for all commands.
data AppEnv = AppEnv
  { appSqlDir :: FilePath
  , appCacheDir :: FilePath
  , appDatabaseUrl :: Maybe ByteString
  , appVerbose :: Bool
  }
  deriving stock (Show)

-- | Load @.env@, then resolve the final 'AppEnv' from CLI options + environment.
resolveEnv
  :: FilePath
  -- ^ SQL directory (from CLI flag)
  -> FilePath
  -- ^ Cache directory (from CLI flag)
  -> Maybe String
  -- ^ Explicit DATABASE_URL (from CLI flag, overrides env)
  -> Bool
  -- ^ Verbose
  -> IO AppEnv
resolveEnv sqlDir cacheDir mDbUrl verbose = do
  -- Best-effort .env loading; ignore errors (file may not exist).
  _ <- try @SomeException $ loadFile defaultConfig
  dbUrl <- case mDbUrl of
    Just url -> pure (Just (BS8.pack url))
    Nothing -> fmap BS8.pack <$> lookupEnv "DATABASE_URL"
  pure
    AppEnv
      { appSqlDir = sqlDir
      , appCacheDir = cacheDir
      , appDatabaseUrl = dbUrl
      , appVerbose = verbose
      }

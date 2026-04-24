module Valiant.CLI.Command.Types
  ( runTypes
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Valiant.CLI.Cache (CacheColumn (..), CacheEntry (..), CacheParam (..), findCacheFile)
import Valiant.CLI.Config (AppEnv (..))
import Valiant.CLI.Discover (SqlFile (..), discoverSqlFiles)
import Valiant.CLI.Error (ValiantCliError (..), dieWithError)
import Valiant.CLI.Output
import System.Exit (ExitCode (..))

runTypes :: AppEnv -> Maybe FilePath -> IO ExitCode
runTypes env mFilter = do
  allFiles <- discoverSqlFiles (appSqlDir env)
  let files = case mFilter of
        Nothing -> allFiles
        Just f -> filter (\sf -> f == sqlRelPath sf) allFiles

  case files of
    [] -> dieWithError (ErrNoSqlFiles (appSqlDir env))
    _ -> pure ()

  results <- mapM (showTypes env) files
  pure $
    if and results
      then ExitSuccess
      else ExitFailure 1

showTypes :: AppEnv -> SqlFile -> IO Bool
showTypes env sqlFile = do
  mEntry <- findCacheFile (appCacheDir env) (sqlRelPath sqlFile) (sqlHash sqlFile)
  case mEntry of
    Nothing -> do
      printLn $ "  " <> T.pack (sqlRelPath sqlFile)
      printLn "    (no cache — run `valiant prepare`)"
      printLn ""
      pure False
    Just entry -> do
      printLn $ "  " <> T.pack (ceFile entry)
      printLn $ "    Params: " <> formatParamType (ceParams entry)
      printLn $ "    Result: " <> formatResultType (ceColumns entry)
      printLn ""
      pure True

formatParamType :: [CacheParam] -> Text
formatParamType [] = "()"
formatParamType [p] = cpHaskellType p
formatParamType ps = "(" <> T.intercalate ", " (map cpHaskellType ps) <> ")"

formatResultType :: [CacheColumn] -> Text
formatResultType [] = "()"
formatResultType [c] = ccHaskellType c
formatResultType cs = "(" <> T.intercalate ", " (map ccHaskellType cs) <> ")"

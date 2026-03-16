module Hsqlx.CLI.Command.Check
  ( runCheck
  ) where

import Data.Text qualified as T
import Hsqlx.CLI.Cache (findCacheFile)
import Hsqlx.CLI.Config (AppEnv (..))
import Hsqlx.CLI.Discover (SqlFile (..), discoverSqlFiles)
import Hsqlx.CLI.Error (HsqlxCliError (..), dieWithError)
import Hsqlx.CLI.Output
import System.Exit (ExitCode (..))

runCheck :: AppEnv -> IO ExitCode
runCheck env = do
  printHeader $ "Checking queries in " <> T.pack (appSqlDir env) <> " ..."

  files <- discoverSqlFiles (appSqlDir env)
  case files of
    [] -> dieWithError (ErrNoSqlFiles (appSqlDir env))
    _ -> pure ()

  let total = length files
  printLn $ "  Checking " <> T.pack (show total) <> " queries..."
  printLn ""

  results <- mapM (checkFile env total) (zip [1 ..] files)
  let okCount = length (filter id results)
  printSummary okCount total

  if okCount == total
    then do
      printLn "  All cache files are current."
      pure ExitSuccess
    else do
      printLn ""
      printLn "  Run `hsqlx prepare` to update stale or missing cache files."
      pure (ExitFailure 1)

checkFile :: AppEnv -> Int -> (Int, SqlFile) -> IO Bool
checkFile env total (idx, sqlFile) = do
  printProgress idx total (sqlRelPath sqlFile)
  mEntry <- findCacheFile (appCacheDir env) (sqlRelPath sqlFile) (sqlHash sqlFile)
  case mEntry of
    Just _ -> do
      printOk
      pure True
    Nothing -> do
      printStale
      pure False

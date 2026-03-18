module Hsqlx.CLI.Command.Watch
  ( runWatch
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Hsqlx.CLI.Command.Prepare (runPrepare)
import Hsqlx.CLI.Config (AppEnv (..))
import Hsqlx.CLI.Discover (SqlFile (..), discoverSqlFiles)
import Hsqlx.CLI.Error (HsqlxCliError (..), dieWithError)
import Hsqlx.CLI.Output
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..))

-- | Run prepare in a loop, watching for SQL file changes.
-- Polls every 2 seconds (fsnotify can be added later for efficiency).
-- Automatically re-prepares when changes are detected.
runWatch :: AppEnv -> IO ExitCode
runWatch env = do
  _ <- case appDatabaseUrl env of
    Nothing -> dieWithError ErrNoDatabaseUrl
    Just _ -> pure ()

  exists <- doesDirectoryExist (appSqlDir env)
  when (not exists) $ dieWithError (ErrSqlDirNotFound (appSqlDir env))

  printHeader $ "Watching " <> T.pack (appSqlDir env) <> " for changes..."
  printLn ""

  -- Initial prepare
  printLn "  Running initial prepare..."
  _ <- runPrepare env []
  printLn ""

  -- Track known file hashes
  files <- discoverSqlFiles (appSqlDir env)
  hashRef <- newIORef (Map.fromList [(sqlRelPath sf, sqlHash sf) | sf <- files])

  -- Poll loop
  _ <- forever $ do
    threadDelay 2000000 -- 2 seconds
    scanAndPrepare env hashRef

  -- unreachable, but needed for type
  pure ExitSuccess

-- | Scan all SQL files, detect changes, report, and re-prepare if needed.
scanAndPrepare :: AppEnv -> IORef (Map FilePath Text) -> IO ()
scanAndPrepare env hashRef = do
  files <- discoverSqlFiles (appSqlDir env)
  oldHashes <- readIORef hashRef

  let newHashes = Map.fromList [(sqlRelPath sf, sqlHash sf) | sf <- files]
      changes = detectChanges oldHashes newHashes

  when (not (null changes)) $ do
    mapM_ reportChange changes
    printLn "  Re-preparing..."
    exitCode <- runPrepare env []
    case exitCode of
      ExitSuccess -> printLn ""
      ExitFailure _ -> printLn "  (Some queries failed. Fix the SQL and save again.)\n"

  -- Update stored hashes
  writeIORef hashRef newHashes

data FileChange
  = FileAdded FilePath
  | FileModified FilePath
  | FileDeleted FilePath

detectChanges :: Map FilePath Text -> Map FilePath Text -> [FileChange]
detectChanges old new =
  let added = [FileAdded k | k <- Map.keys new, not (Map.member k old)]
      modified = [FileModified k | (k, v) <- Map.toList new, Map.member k old, Map.lookup k old /= Just v]
      deleted = [FileDeleted k | k <- Map.keys old, not (Map.member k new)]
   in added ++ modified ++ deleted

reportChange :: FileChange -> IO ()
reportChange (FileAdded path) =
  printLn $ "  [+] " <> T.pack path <> " (new file)"
reportChange (FileModified path) =
  printLn $ "  [~] " <> T.pack path <> " (modified)"
reportChange (FileDeleted path) =
  printLn $ "  [-] " <> T.pack path <> " (deleted)"

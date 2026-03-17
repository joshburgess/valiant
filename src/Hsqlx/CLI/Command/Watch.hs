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
import Hsqlx.CLI.Config (AppEnv (..))
import Hsqlx.CLI.Discover (SqlFile (..), discoverSqlFiles)
import Hsqlx.CLI.Error (HsqlxCliError (..), dieWithError)
import Hsqlx.CLI.Output
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..))

-- | Run prepare in a loop, watching for SQL file changes.
-- Polls every 2 seconds (fsnotify can be added later for efficiency).
runWatch :: AppEnv -> IO ExitCode
runWatch env = do
  _ <- case appDatabaseUrl env of
    Nothing -> dieWithError ErrNoDatabaseUrl
    Just _ -> pure ()

  exists <- doesDirectoryExist (appSqlDir env)
  when (not exists) $ dieWithError (ErrSqlDirNotFound (appSqlDir env))

  printHeader $ "Watching " <> T.pack (appSqlDir env) <> " for changes..."
  printLn ""

  -- Track known file hashes
  hashRef <- newIORef Map.empty

  -- Initial scan
  scanAndReport env hashRef

  -- Poll loop
  _ <- forever $ do
    threadDelay 2000000 -- 2 seconds
    scanAndReport env hashRef

  -- unreachable, but needed for type
  pure ExitSuccess

-- | Scan all SQL files, detect changes, and report.
scanAndReport :: AppEnv -> IORef (Map FilePath Text) -> IO ()
scanAndReport env hashRef = do
  files <- discoverSqlFiles (appSqlDir env)
  oldHashes <- readIORef hashRef

  let newHashes = Map.fromList [(sqlRelPath sf, sqlHash sf) | sf <- files]
      changes = detectChanges oldHashes newHashes

  -- Report changes
  mapM_ (reportChange env) changes

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

reportChange :: AppEnv -> FileChange -> IO ()
reportChange _env (FileAdded path) =
  printLn $ "  [+] " <> T.pack path <> " (new file)"
reportChange _env (FileModified path) =
  printLn $ "  [~] " <> T.pack path <> " (modified)"
reportChange _env (FileDeleted path) =
  printLn $ "  [-] " <> T.pack path <> " (deleted)"

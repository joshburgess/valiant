module Hsqlx.CLI.Discover
  ( SqlFile (..)
  , discoverSqlFiles
  ) where

import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Text (Text)
import Hsqlx.CLI.Hash (sha256Hex, sha256HexTruncated)
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.FilePath (makeRelative)
import System.FilePath.Glob qualified as Glob

-- | A discovered SQL file with its content and hash.
data SqlFile = SqlFile
  { sqlRelPath :: FilePath
  -- ^ Relative path from the SQL root, e.g. @"users/find_by_id.sql"@.
  , sqlAbsPath :: FilePath
  -- ^ Absolute path on disk.
  , sqlContent :: BS.ByteString
  -- ^ Raw file contents.
  , sqlHash :: Text
  -- ^ Full SHA-256 hex digest of the content.
  , sqlHashShort :: Text
  -- ^ Truncated (12-char) SHA-256 hex digest.
  }
  deriving stock (Show)

-- | Recursively discover all @.sql@ files under the given directory.
-- Returns files sorted by relative path.
discoverSqlFiles :: FilePath -> IO [SqlFile]
discoverSqlFiles sqlDir = do
  absDir <- makeAbsolute sqlDir
  exists <- doesDirectoryExist absDir
  if not exists
    then pure []
    else do
      let pat = Glob.compile "**/*.sql"
      matched <- Glob.globDir [pat] absDir
      let paths = sort (concat matched)
      mapM (mkSqlFile absDir) paths

mkSqlFile :: FilePath -> FilePath -> IO SqlFile
mkSqlFile baseDir absPath = do
  content <- BS.readFile absPath
  let relPath = makeRelative baseDir absPath
      hash = sha256Hex content
      hashShort = sha256HexTruncated 12 content
  pure
    SqlFile
      { sqlRelPath = relPath
      , sqlAbsPath = absPath
      , sqlContent = content
      , sqlHash = hash
      , sqlHashShort = hashShort
      }

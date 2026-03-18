module Hsqlx.CLI.Discover
  ( SqlFile (..)
  , discoverSqlFiles
  , discoverInlineSql
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (nub, sort)
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

-- | Scan Haskell source files for @query "..."@ calls and extract the
-- SQL text as synthetic 'SqlFile' entries. Deduplicates by SQL content.
discoverInlineSql :: [FilePath] -> IO [SqlFile]
discoverInlineSql hsPaths = do
  allSqls <- concat <$> mapM extractFromHsFile hsPaths
  -- Deduplicate by content (same SQL text from different files = one cache entry)
  let unique = nub allSqls
  pure (sort unique)

extractFromHsFile :: FilePath -> IO [SqlFile]
extractFromHsFile hsPath = do
  content <- BS.readFile hsPath
  let ls = BS8.lines content
      sqls = concatMap extractQueryCalls ls
  pure [mkInlineSqlFile sql | sql <- sqls]

-- | Simple extraction of query "..." string literals from a line.
-- Looks for: query "..." (handles basic cases, not full Haskell parsing).
extractQueryCalls :: BS.ByteString -> [BS.ByteString]
extractQueryCalls line =
  case BS8.breakSubstring "query \"" line of
    (_, rest)
      | BS.null rest -> []
      | otherwise ->
          let afterQuery = BS.drop 7 rest  -- skip 'query "'
           in case BS8.elemIndex '"' afterQuery of
                Nothing -> []
                Just endIdx ->
                  let sqlText = BS.take endIdx afterQuery
                   in sqlText : extractQueryCalls (BS.drop (endIdx + 1) afterQuery)

mkInlineSqlFile :: BS.ByteString -> SqlFile
mkInlineSqlFile sqlBs =
  let hash = sha256Hex sqlBs
      hashShort = sha256HexTruncated 12 sqlBs
   in SqlFile
        { sqlRelPath = "<inline>"
        , sqlAbsPath = "<inline>"
        , sqlContent = sqlBs
        , sqlHash = hash
        , sqlHashShort = hashShort
        }

instance Eq SqlFile where
  a == b = sqlContent a == sqlContent b

instance Ord SqlFile where
  compare a b = compare (sqlContent a) (sqlContent b)

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

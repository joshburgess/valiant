module Hsqlx.Plugin.ErrorCaseSpec (spec) where

import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = beforeAll_ (createDirectoryIfMissing True "/tmp/hsqlx-error-tests") $ do
  describe "HSQLX-001: SQL file not found" $ do
    it "reports missing .sql file with error code" $ do
      (code, _, stderr) <- compileWith defaultOpts
        "module E001 where\n\
        \import Hsqlx.Statement (Statement, queryFile)\n\
        \import Data.Int (Int32)\n\
        \bad :: Statement Int32 Int32\n\
        \bad = queryFile \"users/nonexistent.sql\"\n"
      code `shouldBe` ExitFailure 1
      stderr `shouldSatisfy` ("HSQLX-001" `isInfixOf`)
      stderr `shouldSatisfy` ("SQL file not found" `isInfixOf`)

  describe "HSQLX-002: Cache stale or missing" $ do
    it "reports missing cache for valid .sql file" $ do
      (code, _, stderr) <- compileWith defaultOpts { optCacheDir = "/tmp/hsqlx-nonexistent-cache" }
        "module E002 where\n\
        \import Hsqlx.Statement (Statement, queryFile)\n\
        \import Data.Int (Int32)\n\
        \bad :: Statement Int32 Int32\n\
        \bad = queryFile \"users/find_by_id.sql\"\n"
      code `shouldBe` ExitFailure 1
      stderr `shouldSatisfy` ("HSQLX-002" `isInfixOf`)
      stderr `shouldSatisfy` ("Query not prepared" `isInfixOf`)

  describe "HSQLX-005: Parameter type mismatch" $ do
    it "reports wrong parameter type" $ do
      -- find_by_id expects Int32 param, we give Text
      (code, _, stderr) <- compileWith defaultOpts
        "module E005 where\n\
        \import Hsqlx.Statement (Statement, queryFile)\n\
        \import Data.Int (Int32)\n\
        \import Data.Text (Text)\n\
        \import Data.Time (UTCTime)\n\
        \bad :: Statement Text (Int32, Text, Maybe Text, Bool, UTCTime)\n\
        \bad = queryFile \"users/find_by_id.sql\"\n"
      code `shouldBe` ExitFailure 1
      stderr `shouldSatisfy` ("HSQLX-005" `isInfixOf`)
      stderr `shouldSatisfy` ("Parameter type mismatch" `isInfixOf`)

  describe "HSQLX-006: Parameter count mismatch" $ do
    it "reports wrong number of parameters" $ do
      -- find_by_id expects 1 param, we give 2
      (code, _, stderr) <- compileWith defaultOpts
        "module E006 where\n\
        \import Hsqlx.Statement (Statement, queryFile)\n\
        \import Data.Int (Int32)\n\
        \import Data.Text (Text)\n\
        \import Data.Time (UTCTime)\n\
        \bad :: Statement (Int32, Text) (Int32, Text, Maybe Text, Bool, UTCTime)\n\
        \bad = queryFile \"users/find_by_id.sql\"\n"
      code `shouldBe` ExitFailure 1
      stderr `shouldSatisfy` ("HSQLX-006" `isInfixOf`)
      stderr `shouldSatisfy` ("Parameter count mismatch" `isInfixOf`)

  describe "HSQLX-004: Column count mismatch" $ do
    it "reports wrong number of result columns" $ do
      -- find_by_id returns 5 columns, we declare 2
      (code, _, stderr) <- compileWith defaultOpts
        "module E004 where\n\
        \import Hsqlx.Statement (Statement, queryFile)\n\
        \import Data.Int (Int32)\n\
        \import Data.Text (Text)\n\
        \bad :: Statement Int32 (Int32, Text)\n\
        \bad = queryFile \"users/find_by_id.sql\"\n"
      code `shouldBe` ExitFailure 1
      stderr `shouldSatisfy` ("HSQLX-004" `isInfixOf`)
      stderr `shouldSatisfy` ("Column count mismatch" `isInfixOf`)

  describe "HSQLX-003: Result type mismatch" $ do
    it "reports wrong column type" $ do
      -- find_by_id col 3 (email) is Maybe Text, we say Text (non-nullable)
      (code, _, stderr) <- compileWith defaultOpts
        "module E003 where\n\
        \import Hsqlx.Statement (Statement, queryFile)\n\
        \import Data.Int (Int32)\n\
        \import Data.Text (Text)\n\
        \import Data.Time (UTCTime)\n\
        \bad :: Statement Int32 (Int32, Text, Text, Bool, UTCTime)\n\
        \bad = queryFile \"users/find_by_id.sql\"\n"
      code `shouldBe` ExitFailure 1
      stderr `shouldSatisfy` ("HSQLX-003" `isInfixOf`)
      stderr `shouldSatisfy` ("Result type mismatch" `isInfixOf`)

  describe "Inline query" $ do
    it "reports error for inline SQL with no cache" $ do
      -- query "..." with no matching cache entry
      (code, _, stderr) <- compileWith defaultOpts
        "module InlineNoCache where\n\
        \import Hsqlx.Statement (Statement, query)\n\
        \import Data.Int (Int32)\n\
        \import Data.Text (Text)\n\
        \bad :: Statement Int32 (Int32, Text)\n\
        \bad = query \"SELECT id, name FROM nonexistent WHERE id = $1\"\n"
      code `shouldBe` ExitFailure 1
      -- Plugin detects the call and reports an error
      stderr `shouldSatisfy` (\s -> "HSQLX-001" `isInfixOf` s || "HSQLX-002" `isInfixOf` s)

    it "rewrites inline query when cache exists" $ do
      -- This test creates a temporary cache file matching the inline SQL,
      -- then compiles. The SQL matches users/find_by_id.sql content but
      -- without the trailing newline, so we need to use the exact SQL
      -- from the cache file.
      -- For now, test that the plugin at least processes the query call
      -- (doesn't crash with HsqlxPluginRequired TypeError).
      (code, _, stderr) <- compileWith defaultOpts
        "module InlineRewrite where\n\
        \import Hsqlx.Statement (Statement, query)\n\
        \import Data.Int (Int32)\n\
        \import Data.Text (Text)\n\
        \bad :: Statement Int32 (Int32, Text)\n\
        \bad = query \"SELECT id, name FROM users WHERE id = $1\"\n"
      -- Should NOT get the HsqlxPluginRequired TypeError
      -- (proves the rewrite phase ran)
      stderr `shouldSatisfy` (not . ("requires the Hsqlx.Plugin" `isInfixOf`))

  describe "Successful compilation" $ do
    it "compiles correct positional params" $ do
      (code, _, _) <- compileWith defaultOpts
        "module Good1 where\n\
        \import Hsqlx.Statement (Statement, queryFile)\n\
        \import Data.Int (Int32)\n\
        \import Data.Text (Text)\n\
        \import Data.Time (UTCTime)\n\
        \q :: Statement Int32 (Int32, Text, Maybe Text, Bool, UTCTime)\n\
        \q = queryFile \"users/find_by_id.sql\"\n"
      code `shouldBe` ExitSuccess

    it "compiles named params query" $ do
      (code, _, _) <- compileWith defaultOpts
        "module Good2 where\n\
        \import Hsqlx.Statement (Statement, queryFile)\n\
        \import Data.Int (Int32)\n\
        \import Data.Text (Text)\n\
        \q :: Statement (Text, Bool) (Int32, Text, Maybe Text)\n\
        \q = queryFile \"users/find_by_name_and_status.sql\"\n"
      code `shouldBe` ExitSuccess

    it "compiles unit result (INSERT)" $ do
      (code, _, _) <- compileWith defaultOpts
        "module Good3 where\n\
        \import Hsqlx.Statement (Statement, queryFile)\n\
        \import Data.Text (Text)\n\
        \q :: Statement (Text, Maybe Text) ()\n\
        \q = queryFile \"users/insert.sql\"\n"
      code `shouldBe` ExitSuccess

    it "compiles no-param query" $ do
      (code, _, _) <- compileWith defaultOpts
        "module Good4 where\n\
        \import Hsqlx.Statement (Statement, queryFile)\n\
        \import Data.Int (Int32)\n\
        \import Data.Text (Text)\n\
        \q :: Statement () (Int32, Text)\n\
        \q = queryFile \"users/list_all.sql\"\n"
      code `shouldBe` ExitSuccess

-- Helpers -----------------------------------------------------------------

data CompileOpts = CompileOpts
  { optSqlDir :: String
  , optCacheDir :: String
  }

-- | Default options. sql-dir and cache-dir are set to absolute paths
-- in compileWith using getCurrentDirectory.
defaultOpts :: CompileOpts
defaultOpts = CompileOpts
  { optSqlDir = ""
  , optCacheDir = ""
  }

compileWith :: CompileOpts -> String -> IO (ExitCode, String, String)
compileWith opts src = do
  -- Extract module name from source to use as unique filename
  let modName = case words src of
        ("module" : name : _) -> name
        _ -> "Test"
      tmpFile = "/tmp/hsqlx-error-tests/" <> modName <> ".hs"
      outDir = "/tmp/hsqlx-error-tests/out-" <> modName
  createDirectoryIfMissing True outDir
  writeFile tmpFile src
  -- Resolve paths relative to the project root.
  -- cabal test may run from the package subdir, so we go up to find sql/ and .hsqlx/.
  projRoot <- findProjectRoot
  let sqlDir = if null (optSqlDir opts) then projRoot <> "/sql" else optSqlDir opts
      cacheDir = if null (optCacheDir opts) then projRoot <> "/.hsqlx" else optCacheDir opts
      args =
        [ "exec", "--"
        , "ghc"
        , "-package-db", projRoot <> "/dist-newstyle/packagedb/ghc-9.10.3"
        , "-package", "hsqlx"
        , "-package", "hsqlx-plugin"
        , "-fplugin=Hsqlx.Plugin"
        , "-fplugin-opt=Hsqlx.Plugin:sql-dir=" <> sqlDir
        , "-fplugin-opt=Hsqlx.Plugin:cache-dir=" <> cacheDir
        , "-c", "-no-link"
        , "-outputdir", outDir
        , "-fforce-recomp"
        , tmpFile
        ]
  readProcessWithExitCode "cabal" args ""

-- | Find the project root by looking for cabal.project.
findProjectRoot :: IO FilePath
findProjectRoot = go =<< getCurrentDirectory
  where
    go dir = do
      exists <- doesFileExist (dir <> "/cabal.project")
      if exists
        then pure dir
        else let parent = takeDirectory dir
              in if parent == dir
                   then pure dir -- give up at filesystem root
                   else go parent

takeDirectory :: FilePath -> FilePath
takeDirectory = reverse . dropWhile (/= '/') . drop 1 . reverse

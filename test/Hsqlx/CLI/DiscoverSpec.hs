module Hsqlx.CLI.DiscoverSpec (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.List (sort)
import Hsqlx.CLI.Discover
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "discoverSqlFiles" $ do
    it "finds .sql files recursively" $ do
      withSystemTempDirectory "hsqlx-test" $ \tmpDir -> do
        createDirectoryIfMissing True (tmpDir </> "users")
        BS8.writeFile (tmpDir </> "users" </> "find.sql") "SELECT 1"
        BS8.writeFile (tmpDir </> "list.sql") "SELECT 2"
        files <- discoverSqlFiles tmpDir
        sort (map sqlRelPath files) `shouldBe` ["list.sql", "users/find.sql"]

    it "returns empty for non-existent directory" $ do
      files <- discoverSqlFiles "/tmp/hsqlx-nonexistent-dir-12345"
      map sqlRelPath files `shouldBe` []

    it "ignores non-.sql files" $ do
      withSystemTempDirectory "hsqlx-test" $ \tmpDir -> do
        BS8.writeFile (tmpDir </> "query.sql") "SELECT 1"
        BS8.writeFile (tmpDir </> "notes.txt") "not sql"
        BS8.writeFile (tmpDir </> "readme.md") "not sql"
        files <- discoverSqlFiles tmpDir
        map sqlRelPath files `shouldBe` ["query.sql"]

    it "reads file content correctly" $ do
      withSystemTempDirectory "hsqlx-test" $ \tmpDir -> do
        let content = "SELECT id FROM users WHERE id = $1"
        BS8.writeFile (tmpDir </> "test.sql") content
        files <- discoverSqlFiles tmpDir
        case files of
          [f] -> sqlContent f `shouldBe` content
          _ -> expectationFailure "expected exactly one file"

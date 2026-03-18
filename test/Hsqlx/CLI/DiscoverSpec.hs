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

  describe "discoverInlineSql" $ do
    it "extracts query calls from Haskell source" $ do
      withSystemTempDirectory "hsqlx-test" $ \tmpDir -> do
        let hsFile = tmpDir </> "Queries.hs"
        writeFile hsFile $ unlines
          [ "module Queries where"
          , "import Hsqlx"
          , "findById = query \"SELECT id, name FROM users WHERE id = $1\""
          , "listAll = query \"SELECT id, name FROM users\""
          ]
        files <- discoverInlineSql [hsFile]
        length files `shouldBe` 2

    it "extracts SQL content correctly" $ do
      withSystemTempDirectory "hsqlx-test" $ \tmpDir -> do
        let hsFile = tmpDir </> "Q.hs"
        writeFile hsFile "q = query \"SELECT 1\""
        files <- discoverInlineSql [hsFile]
        case files of
          [f] -> sqlContent f `shouldBe` "SELECT 1"
          _ -> expectationFailure $ "expected 1 file, got " <> show (length files)

    it "sets path to <inline>" $ do
      withSystemTempDirectory "hsqlx-test" $ \tmpDir -> do
        let hsFile = tmpDir </> "Q.hs"
        writeFile hsFile "q = query \"SELECT 1\""
        files <- discoverInlineSql [hsFile]
        case files of
          [f] -> sqlRelPath f `shouldBe` "<inline>"
          _ -> expectationFailure "expected 1 file"

    it "deduplicates identical SQL across files" $ do
      withSystemTempDirectory "hsqlx-test" $ \tmpDir -> do
        let f1 = tmpDir </> "A.hs"
            f2 = tmpDir </> "B.hs"
        writeFile f1 "a = query \"SELECT 1\""
        writeFile f2 "b = query \"SELECT 1\""
        files <- discoverInlineSql [f1, f2]
        length files `shouldBe` 1

    it "handles multiple queries on separate lines" $ do
      withSystemTempDirectory "hsqlx-test" $ \tmpDir -> do
        let hsFile = tmpDir </> "Q.hs"
        writeFile hsFile $ unlines
          [ "a = query \"SELECT 1\""
          , "b = query \"SELECT 2\""
          , "c = query \"SELECT 3\""
          ]
        files <- discoverInlineSql [hsFile]
        length files `shouldBe` 3

    it "ignores queryFile calls" $ do
      withSystemTempDirectory "hsqlx-test" $ \tmpDir -> do
        let hsFile = tmpDir </> "Q.hs"
        writeFile hsFile "q = queryFile \"users/find.sql\""
        files <- discoverInlineSql [hsFile]
        length files `shouldBe` 0

    it "ignores lines without query calls" $ do
      withSystemTempDirectory "hsqlx-test" $ \tmpDir -> do
        let hsFile = tmpDir </> "Q.hs"
        writeFile hsFile $ unlines
          [ "module Q where"
          , "import Hsqlx"
          , "-- this is a comment with query in it"
          , "x = 42"
          ]
        files <- discoverInlineSql [hsFile]
        length files `shouldBe` 0

    it "returns empty for no .hs files" $ do
      files <- discoverInlineSql []
      length files `shouldBe` 0

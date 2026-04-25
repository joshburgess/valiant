module Valiant.Plugin.CacheSpec (spec) where

import Valiant.Plugin.Cache
import System.Directory (doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "cacheFileName" $ do
    it "produces the expected format" $ do
      cacheFileName "users/find_by_id.sql" "a1b2c3d4e5f6"
        `shouldBe` "users-find_by_id-a1b2c3d4e5f6.json"

    it "handles nested paths" $ do
      cacheFileName "admin/reports/monthly.sql" "abcdef012345"
        `shouldBe` "admin-reports-monthly-abcdef012345.json"

    it "handles single-level paths" $ do
      cacheFileName "query.sql" "abc123abc123"
        `shouldBe` "query-abc123abc123.json"

  describe "findCacheFile" $ do
    it "returns Nothing for non-existent cache directory" $ do
      result <- findCacheFile "/tmp/valiant-nonexistent-cache-dir" "test.sql" "abc123"
      result `shouldBe` Nothing

    it "returns Nothing when no matching file exists" $ do
      withSystemTempDirectory "valiant-cache-test" $ \tmpDir -> do
        result <- findCacheFile tmpDir "test.sql" "abc123"
        result `shouldBe` Nothing

  describe "readCacheEntry" $ do
    it "reads an actual .valiant cache file written by the CLI" $ do
      -- Tests compatibility between CLI-written cache and plugin reader
      let cacheFile = "/Users/joshburgess/code/valiant/.valiant/posts-find_by_id-b63f2b90e455.json"
      exists <- doesFileExist cacheFile
      if not exists
        then pendingWith "No .valiant cache files available (run valiant prepare first)"
        else do
          result <- readCacheEntry cacheFile
          case result of
            Right entry -> do
              ceFile entry `shouldBe` "posts/find_by_id.sql"
              ceVersion entry `shouldBe` "0.1.0"
              length (ceParams entry) `shouldBe` 1
              length (ceColumns entry) `shouldSatisfy` (> 0)
            Left err -> expectationFailure $ "Failed to parse cache file: " <> err

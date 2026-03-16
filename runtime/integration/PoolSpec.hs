module PoolSpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Hsqlx
import TestSupport
import Test.Hspec

spec :: Spec
spec = do
  describe "withResource" $ do
    it "acquires and releases a connection" $ do
      withTestPool $ \pool -> do
        result <- withResource pool $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT 42"
          pure (length rows)
        result `shouldBe` 1

    it "handles concurrent access" $ do
      withTestPool $ \pool -> do
        results <- mapConcurrently (\_ -> withResource pool $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT 1"
          pure (length rows)) [1 :: Int .. 20]
        all (== 1) results `shouldBe` True

    it "reuses connections" $ do
      withTestPool $ \pool -> do
        pid1 <- withResource pool $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT pg_backend_pid()"
          pure rows
        pid2 <- withResource pool $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT pg_backend_pid()"
          pure rows
        pid1 `shouldBe` pid2

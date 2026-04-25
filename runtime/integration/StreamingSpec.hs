module StreamingSpec (spec) where

import Data.Int (Int32)
import Data.Text (Text)
import Valiant
import TestSupport
import Test.Hspec

stmtListAll :: Statement () (Int32, Text)
stmtListAll = mkStatement
  "SELECT id, name FROM users ORDER BY id"
  [] ["id", "name"] "<test>"

stmtListByPrefix :: Statement Text (Int32, Text)
stmtListByPrefix = mkStatement
  "SELECT id, name FROM users WHERE name LIKE $1 ORDER BY id"
  [25] ["id", "name"] "<test>"

spec :: Spec
spec = do
  describe "withCursor / fetchBatch" $ do
    it "drains a small result set across multiple batches" $ do
      withTestPool $ \pool -> do
        withTransaction pool $ \tx -> withSchema (txConn tx) $ do
          insertTestUsers (txConn tx)
          rows <- withCursor (txConn tx) stmtListAll () 2 $ \cs ->
            let loop !acc = do
                  batch <- fetchBatch cs 2
                  if null batch
                    then pure (reverse acc)
                    else loop (batch : acc)
            in loop []
          length (concat rows) `shouldBe` 5

    it "returns empty after exhaustion without re-querying" $ do
      withTestPool $ \pool -> do
        withTransaction pool $ \tx -> withSchema (txConn tx) $ do
          insertTestUsers (txConn tx)
          (firstBatch, secondBatch, thirdBatch) <-
            withCursor (txConn tx) stmtListAll () 100 $ \cs -> do
              b1 <- fetchBatch cs 100
              b2 <- fetchBatch cs 100
              b3 <- fetchBatch cs 100
              pure (b1, b2, b3)
          length firstBatch `shouldBe` 5
          secondBatch `shouldBe` []
          thirdBatch `shouldBe` []

    it "respects parameter binding" $ do
      withTestPool $ \pool -> do
        withTransaction pool $ \tx -> withSchema (txConn tx) $ do
          insertTestUsers (txConn tx)
          rows <- withCursor (txConn tx) stmtListByPrefix "A%" 100 $ \cs ->
            fetchBatch cs 100
          length rows `shouldBe` 1

  describe "fetchAllCursor" $ do
    it "materialises every row in order" $ do
      withTestPool $ \pool -> do
        withTransaction pool $ \tx -> withSchema (txConn tx) $ do
          insertBulkUsers (txConn tx) 1000
          rows <- fetchAllCursor (txConn tx) stmtListAll () 100
          length rows `shouldBe` 1000
          map fst rows `shouldBe` [1 .. 1000]

    it "handles batch sizes larger than the result set" $ do
      withTestPool $ \pool -> do
        withTransaction pool $ \tx -> withSchema (txConn tx) $ do
          insertTestUsers (txConn tx)
          rows <- fetchAllCursor (txConn tx) stmtListAll () 10000
          length rows `shouldBe` 5

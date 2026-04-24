module Main where

import Data.Int (Int32)
import Data.Text (Text)
import Valiant
import Valiant.Streamly (foldStreamly, selectStreamly)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import Test.Hspec
import TestSupport

main :: IO ()
main = hspec $ do
  describe "Valiant.Streamly" $ do
    describe "selectStreamly (cursor-based)" $ do
      it "streams all rows from a cursor" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          withTransactionConn conn $ \tx -> do
            rows <- Stream.fold Fold.toList $
              selectStreamly (txConn tx) stmtListAll () 2
            length rows `shouldBe` 5

      it "returns empty for no rows" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          withTransactionConn conn $ \tx -> do
            rows <- Stream.fold Fold.toList $
              selectStreamly (txConn tx) stmtListAll () 10
            rows `shouldBe` ([] :: [(Int32, Text)])

      it "works with small batch size" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          withTransactionConn conn $ \tx -> do
            rows <- Stream.fold Fold.toList $
              selectStreamly (txConn tx) stmtListAll () 1
            length rows `shouldBe` 5

    describe "foldStreamly (no transaction)" $ do
      it "streams all rows without a transaction" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          rows <- Stream.fold Fold.toList $
            foldStreamly conn stmtListAll ()
          length rows `shouldBe` 5

      it "returns empty for no rows" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          rows <- Stream.fold Fold.toList $
            foldStreamly conn stmtListAll ()
          rows `shouldBe` ([] :: [(Int32, Text)])

      it "composes with streamly combinators" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          count <- Stream.fold Fold.length $
            foldStreamly conn stmtListAll ()
          count `shouldBe` 5

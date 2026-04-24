module Main where

import Data.Int (Int32)
import Data.Text (Text)
import Valiant
import Valiant.Pipes (foldPipe, selectPipe)
import Pipes ((>->))
import Pipes.Prelude qualified as P
import Test.Hspec
import TestSupport

main :: IO ()
main = hspec $ do
  describe "Valiant.Pipes" $ do
    describe "selectPipe (cursor-based)" $ do
      it "streams all rows from a cursor" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          withTransactionConn conn $ \tx -> do
            rows <- P.toListM $
              selectPipe (txConn tx) stmtListAll () 2
            length rows `shouldBe` 5

      it "returns empty for no rows" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          withTransactionConn conn $ \tx -> do
            rows <- P.toListM $
              selectPipe (txConn tx) stmtListAll () 10
            rows `shouldBe` ([] :: [(Int32, Text)])

      it "works with small batch size" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          withTransactionConn conn $ \tx -> do
            rows <- P.toListM $
              selectPipe (txConn tx) stmtListAll () 1
            length rows `shouldBe` 5

    describe "foldPipe (no transaction)" $ do
      it "streams all rows without a transaction" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          rows <- P.toListM $ foldPipe conn stmtListAll ()
          length rows `shouldBe` 5

      it "returns empty for no rows" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          rows <- P.toListM $ foldPipe conn stmtListAll ()
          rows `shouldBe` ([] :: [(Int32, Text)])

      it "composes with pipes combinators" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          names <- P.toListM $
            foldPipe conn stmtListAll ()
            >-> P.map snd
          length names `shouldBe` 5
          take 1 names `shouldBe` ["Alice"]

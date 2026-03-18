module Main where

import Data.Int (Int32)
import Data.Text (Text)
import Hsqlx
import Hsqlx.Stream (foldStream, selectStream)
import Streaming.Prelude qualified as S
import Test.Hspec
import TestSupport

main :: IO ()
main = hspec $ do
  describe "Hsqlx.Stream" $ do
    describe "selectStream (cursor-based)" $ do
      it "streams all rows from a cursor" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          withTransactionConn conn $ \tx -> do
            rows <- S.toList_ $
              selectStream (txConn tx) stmtListAll () 2
            length rows `shouldBe` 5

      it "returns empty for no rows" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          withTransactionConn conn $ \tx -> do
            rows <- S.toList_ $
              selectStream (txConn tx) stmtListAll () 10
            rows `shouldBe` ([] :: [(Int32, Text)])

      it "works with small batch size" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          withTransactionConn conn $ \tx -> do
            rows <- S.toList_ $
              selectStream (txConn tx) stmtListAll () 1
            length rows `shouldBe` 5

    describe "foldStream (no transaction)" $ do
      it "streams all rows without a transaction" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          rows <- S.toList_ $
            foldStream conn stmtListAll ()
          length rows `shouldBe` 5

      it "returns empty for no rows" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          rows <- S.toList_ $
            foldStream conn stmtListAll ()
          rows `shouldBe` ([] :: [(Int32, Text)])

      it "composes with streaming combinators" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          names <- S.toList_ $
            S.map snd $
            foldStream conn stmtListAll ()
          length names `shouldBe` 5
          take 1 names `shouldBe` ["Alice"]

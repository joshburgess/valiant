module Main where

import Data.Conduit (runConduit, (.|))
import Data.Conduit.List qualified as CL
import Data.Int (Int32)
import Data.Text (Text)
import Hsqlx
import Hsqlx.Conduit (foldSource, selectSource)
import Test.Hspec
import TestSupport

main :: IO ()
main = hspec $ do
  describe "Hsqlx.Conduit" $ do
    describe "selectSource (cursor-based)" $ do
      it "streams all rows from a cursor" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          withTransactionConn conn $ \tx -> do
            rows <- runConduit $
              selectSource (txConn tx) stmtListAll () 2
              .| CL.consume
            length rows `shouldBe` 5

      it "returns empty for no rows" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          withTransactionConn conn $ \tx -> do
            rows <- runConduit $
              selectSource (txConn tx) stmtListAll () 10
              .| CL.consume
            rows `shouldBe` ([] :: [(Int32, Text)])

      it "respects batch size by returning correct total" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          withTransactionConn conn $ \tx -> do
            rows <- runConduit $
              selectSource (txConn tx) stmtListAll () 1
              .| CL.consume
            length rows `shouldBe` 5

    describe "foldSource (no transaction)" $ do
      it "streams all rows without a transaction" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          rows <- runConduit $
            foldSource conn stmtListAll ()
            .| CL.consume
          length rows `shouldBe` 5

      it "returns empty for no rows" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          rows <- runConduit $
            foldSource conn stmtListAll ()
            .| CL.consume
          rows `shouldBe` ([] :: [(Int32, Text)])

      it "composes with conduit combinators" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          names <- runConduit $
            foldSource conn stmtListAll ()
            .| CL.map snd
            .| CL.consume
          length names `shouldBe` 5
          take 1 names `shouldBe` ["Alice"]

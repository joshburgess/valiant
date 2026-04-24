module CopySpec (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Valiant
import TestSupport
import Test.Hspec

spec :: Spec
spec = do
  describe "copyIn" $ do
    it "bulk inserts via COPY FROM STDIN" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        result <- copyIn conn
          "COPY users (name, email) FROM STDIN WITH (FORMAT csv)"
          $ \send -> do
            send "Alice,alice@example.com\n"
            send "Bob,bob@example.com\n"
            send "Carol,\n"
        copyRows result `shouldBe` 3

  describe "copyOut" $ do
    it "exports via COPY TO STDOUT" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        _ <- simpleQuery conn
          "INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com'), ('Bob', 'bob@example.com')"

        chunks <- newIORef ([] :: [BS8.ByteString])
        _ <- copyOut conn
          "COPY users (name, email) TO STDOUT WITH (FORMAT csv)"
          $ \chunk -> modifyIORef' chunks (chunk :)

        allData <- BS8.concat . reverse <$> readIORef chunks
        BS8.isInfixOf "Alice" allData `shouldBe` True
        BS8.isInfixOf "Bob" allData `shouldBe` True

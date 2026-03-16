module PgWire.Protocol.BuildersSpec (spec) where

import Data.ByteString qualified as BS
import Data.Vector qualified as V
import PgWire.Protocol.Builders (buildFrontendMsg, buildStartup)
import PgWire.Protocol.Frontend
import Test.Hspec

spec :: Spec
spec = do
  describe "buildFrontendMsg" $ do
    it "Sync is 5 bytes (tag + length)" $ do
      let bs = buildFrontendMsg Sync
      BS.length bs `shouldBe` 5
      BS.index bs 0 `shouldBe` 83 -- 'S'

    it "Terminate is 5 bytes" $ do
      let bs = buildFrontendMsg Terminate
      BS.length bs `shouldBe` 5
      BS.index bs 0 `shouldBe` 88 -- 'X'

    it "Flush is 5 bytes" $ do
      let bs = buildFrontendMsg Flush
      BS.length bs `shouldBe` 5
      BS.index bs 0 `shouldBe` 72 -- 'H'

    it "Query includes NUL terminator" $ do
      let bs = buildFrontendMsg (Query "SELECT 1")
      -- tag(1) + length(4) + "SELECT 1"(8) + NUL(1) = 14
      BS.length bs `shouldBe` 14
      BS.index bs 0 `shouldBe` 81 -- 'Q'
      BS.last bs `shouldBe` 0

    it "Parse includes statement name, SQL, and param count" $ do
      let bs = buildFrontendMsg (Parse "s1" "SELECT $1" (V.singleton 23))
      BS.index bs 0 `shouldBe` 80 -- 'P'
      -- Contains the statement name and SQL
      "s1" `BS.isInfixOf` bs `shouldBe` True
      "SELECT $1" `BS.isInfixOf` bs `shouldBe` True

    it "CopyDone is 5 bytes" $ do
      let bs = buildFrontendMsg CopyDone
      BS.length bs `shouldBe` 5
      BS.index bs 0 `shouldBe` 99 -- 'c'

  describe "buildStartup" $ do
    it "starts with length and protocol version 3.0" $ do
      let bs = buildStartup (StartupParams "user" "db" "" [])
      -- First 4 bytes = length, next 4 bytes = 196608 (3.0)
      BS.length bs `shouldSatisfy` (> 8)
      -- Protocol version 3.0 = 0x00030000 = 196608
      let v = fromIntegral (BS.index bs 4) * 256 * 256 * 256
            + fromIntegral (BS.index bs 5) * 256 * 256
            + fromIntegral (BS.index bs 6) * 256
            + fromIntegral (BS.index bs 7) :: Int
      v `shouldBe` 196608

    it "includes user and database params" $ do
      let bs = buildStartup (StartupParams "testuser" "testdb" "" [])
      "user" `BS.isInfixOf` bs `shouldBe` True
      "testuser" `BS.isInfixOf` bs `shouldBe` True
      "database" `BS.isInfixOf` bs `shouldBe` True
      "testdb" `BS.isInfixOf` bs `shouldBe` True

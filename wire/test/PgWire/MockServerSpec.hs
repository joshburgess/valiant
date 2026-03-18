module PgWire.MockServerSpec (spec) where

import Data.ByteString.Char8 qualified as BS8
import PgWire.Connection (Connection, close, connectString, simpleQuery)
import PgWire.MockServer
import Test.Hspec

connectToMock :: MockConfig -> (Connection -> IO a) -> IO a
connectToMock cfg action =
  withMockServer cfg $ \port -> do
    let connStr = "postgres://testuser:testpass@127.0.0.1:" <> BS8.pack (show port) <> "/testdb"
    conn <- connectString connStr
    result <- action conn
    close conn
    pure result

spec :: Spec
spec = do
  describe "MockServer" $ do
    it "accepts a connection and responds to simple query" $ do
      connectToMock defaultMockConfig $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT 1"
        -- Default handler returns CommandComplete "SELECT 0" with no rows
        rows `shouldBe` []

    it "returns rows from a custom handler" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = simpleHandler
                [ ("SELECT name FROM test",
                   [ [("name", "Alice")]
                   , [("name", "Bob")]
                   ])
                ]
            }
      connectToMock cfg $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT name FROM test"
        length rows `shouldBe` 2

    it "handles unknown queries gracefully" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = simpleHandler []
            }
      connectToMock cfg $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT nothing"
        rows `shouldBe` []

    it "supports multiple sequential queries" $ do
      connectToMock defaultMockConfig $ \conn -> do
        _ <- simpleQuery conn "SELECT 1"
        _ <- simpleQuery conn "SELECT 2"
        (rows, _) <- simpleQuery conn "SELECT 3"
        rows `shouldBe` []

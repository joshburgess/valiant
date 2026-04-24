module PgWire.MockServerSpec (spec) where

import Control.Exception (try)
import Data.ByteString.Char8 qualified as BS8
import PgWire.Connection (Connection, close, connectString, simpleQuery)
import PgWire.Error (PgWireError (..))
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
    describe "basic protocol" $ do
      it "accepts a connection and responds to simple query" $ do
        connectToMock defaultMockConfig $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT 1"
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

    describe "error responses" $ do
      it "propagates server errors as QueryError" $ do
        let cfg = defaultMockConfig
              { mockQueryHandler = errorHandler "42P01" "relation \"foo\" does not exist"
              }
        connectToMock cfg $ \conn -> do
          result <- try @PgWireError (simpleQuery conn "SELECT * FROM foo")
          case result of
            Left (QueryError _) -> pure ()
            Left other -> expectationFailure $ "Expected QueryError, got: " <> show other
            Right _ -> expectationFailure "Expected an error"

      it "recovers after an error and accepts subsequent queries" $ do
        let cfg = defaultMockConfig
              { mockQueryHandler = \sql send ->
                  if sql == "BAD"
                    then send $ buildErrorResponse "42601" "syntax error"
                    else send $ buildCommandComplete "SELECT 0"
              }
        connectToMock cfg $ \conn -> do
          -- First query errors
          result1 <- try @PgWireError (simpleQuery conn "BAD")
          case result1 of
            Left (QueryError _) -> pure ()
            _ -> expectationFailure "Expected QueryError for BAD query"
          -- Second query succeeds on the same connection
          (rows, _) <- simpleQuery conn "GOOD"
          rows `shouldBe` []

    describe "multi-row results" $ do
      it "returns correct number of rows for large result sets" $ do
        let manyRows = [ [("id", BS8.pack (show i)), ("val", "x")]
                        | i <- [1 :: Int .. 100]
                        ]
            cfg = defaultMockConfig
              { mockQueryHandler = simpleHandler
                  [("SELECT id, val FROM big", manyRows)]
              }
        connectToMock cfg $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT id, val FROM big"
          length rows `shouldBe` 100

      it "returns multiple columns correctly" $ do
        let cfg = defaultMockConfig
              { mockQueryHandler = simpleHandler
                  [("SELECT a, b, c FROM t",
                    [ [("a", "1"), ("b", "hello"), ("c", "true")]
                    , [("a", "2"), ("b", "world"), ("c", "false")]
                    ])]
              }
        connectToMock cfg $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT a, b, c FROM t"
          length rows `shouldBe` 2
          case rows of
            [row1, row2] -> do
              length row1 `shouldBe` 3
              length row2 `shouldBe` 3
            _ -> expectationFailure "Expected exactly 2 rows"

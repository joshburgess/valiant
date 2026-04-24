module Valiant.ErrorSpec (spec) where

import Control.Exception (try)
import Data.ByteString.Char8 qualified as BS8
import Valiant.Error
import PgWire.Connection (Connection, close, connectString, simpleQuery)
import PgWire.Error (PgWireError (..))
import PgWire.MockServer
import Test.Hspec

withMockConn :: MockConfig -> (Connection -> IO a) -> IO a
withMockConn cfg action =
  withMockServer cfg $ \port -> do
    let connStr = "postgres://testuser:testpass@127.0.0.1:" <> BS8.pack (show port) <> "/testdb"
    conn <- connectString connStr
    result <- action conn
    close conn
    pure result

triggerError :: Connection -> IO PgWireError
triggerError conn = do
  result <- try (simpleQuery conn "TRIGGER ERROR")
  case result of
    Left err -> pure err
    Right _ -> error "Expected an error but got a result"

spec :: Spec
spec = do
  describe "sqlState" $ do
    it "extracts SQLSTATE from a query error" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = errorHandler "42P01" "relation does not exist"
            }
      withMockConn cfg $ \conn -> do
        err <- triggerError conn
        sqlState err `shouldBe` Just "42P01"

    it "returns Nothing for non-query errors" $ do
      sqlState (DecodeError "test") `shouldBe` Nothing
      sqlState PoolTimeout `shouldBe` Nothing
      sqlState PoolClosed `shouldBe` Nothing

  describe "constraint violation predicates" $ do
    it "detects unique violation (23505)" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = errorHandler "23505" "duplicate key"
            }
      withMockConn cfg $ \conn -> do
        err <- triggerError conn
        isUniqueViolation err `shouldBe` True
        isForeignKeyViolation err `shouldBe` False

    it "detects foreign key violation (23503)" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = errorHandler "23503" "violates foreign key"
            }
      withMockConn cfg $ \conn -> do
        err <- triggerError conn
        isForeignKeyViolation err `shouldBe` True
        isUniqueViolation err `shouldBe` False

    it "detects not-null violation (23502)" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = errorHandler "23502" "null value"
            }
      withMockConn cfg $ \conn -> do
        err <- triggerError conn
        isNotNullViolation err `shouldBe` True

    it "detects check violation (23514)" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = errorHandler "23514" "check constraint"
            }
      withMockConn cfg $ \conn -> do
        err <- triggerError conn
        isCheckViolation err `shouldBe` True

  describe "transaction error predicates" $ do
    it "detects serialization failure (40001)" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = errorHandler "40001" "could not serialize"
            }
      withMockConn cfg $ \conn -> do
        err <- triggerError conn
        isSerializationError err `shouldBe` True
        isDeadlockError err `shouldBe` False

    it "detects deadlock (40P01)" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = errorHandler "40P01" "deadlock detected"
            }
      withMockConn cfg $ \conn -> do
        err <- triggerError conn
        isDeadlockError err `shouldBe` True

  describe "other error predicates" $ do
    it "detects undefined table (42P01)" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = errorHandler "42P01" "relation does not exist"
            }
      withMockConn cfg $ \conn -> do
        err <- triggerError conn
        isUndefinedTable err `shouldBe` True

    it "detects syntax error (42601)" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = errorHandler "42601" "syntax error"
            }
      withMockConn cfg $ \conn -> do
        err <- triggerError conn
        isSyntaxError err `shouldBe` True

  describe "constraintViolation" $ do
    it "extracts UniqueViolation from 23505" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = errorHandler "23505" "duplicate key"
            }
      withMockConn cfg $ \conn -> do
        err <- triggerError conn
        case constraintViolation err of
          Just (UniqueViolation _) -> pure ()
          other -> expectationFailure $ "Expected UniqueViolation, got: " <> show other

    it "returns Nothing for non-constraint errors" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = errorHandler "42P01" "relation does not exist"
            }
      withMockConn cfg $ \conn -> do
        err <- triggerError conn
        constraintViolation err `shouldBe` Nothing

  describe "catchConstraintViolation" $ do
    it "catches and handles constraint violations" $ do
      let cfg = defaultMockConfig
            { mockQueryHandler = errorHandler "23505" "duplicate key"
            }
      withMockConn cfg $ \conn -> do
        result <- catchConstraintViolation
          (\_ cv -> case cv of
            UniqueViolation _ -> pure ("caught" :: String)
            _ -> pure "wrong type")
          (simpleQuery conn "INSERT" >> pure "no error")
        result `shouldBe` ("caught" :: String)

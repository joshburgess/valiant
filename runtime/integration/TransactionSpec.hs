module TransactionSpec (spec) where

import Control.Exception (SomeException, try)
import Hsqlx
import TestSupport
import Test.Hspec

spec :: Spec
spec = do
  describe "withTransaction" $ do
    it "commits on success" $ do
      withTestPool $ \pool -> do
        withResource pool $ \conn -> do
          dropSchema conn
          createSchema conn

        _ <- withTransaction pool $ \tx ->
          simpleQuery (txConn tx) "INSERT INTO users (name) VALUES ('Alice')"

        count <- withResource pool $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT count(*) FROM users"
          pure rows
        case count of
          [[Just n]] -> n `shouldBe` "1"
          _ -> expectationFailure $ "Unexpected: " <> show count

        withResource pool $ \conn -> dropSchema conn

    it "rolls back on exception" $ do
      withTestPool $ \pool -> do
        withResource pool $ \conn -> do
          dropSchema conn
          createSchema conn

        _ <- try @SomeException $ withTransaction pool $ \tx -> do
          _ <- simpleQuery (txConn tx) "INSERT INTO users (name) VALUES ('Bob')"
          error "intentional failure"

        count <- withResource pool $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT count(*) FROM users"
          pure rows
        case count of
          [[Just n]] -> n `shouldBe` "0"
          _ -> expectationFailure $ "Unexpected: " <> show count

        withResource pool $ \conn -> dropSchema conn

  describe "withSavepoint" $ do
    it "preserves outer transaction on savepoint failure" $ do
      withTestPool $ \pool -> do
        withResource pool $ \conn -> do
          dropSchema conn
          createSchema conn

        withTransaction pool $ \tx -> do
          _ <- simpleQuery (txConn tx) "INSERT INTO users (name) VALUES ('Alice')"

          -- This savepoint should fail but not kill the transaction
          _ <- try @SomeException $ withSavepoint tx $ \_ -> do
            _ <- simpleQuery (txConn tx) "INSERT INTO users (name) VALUES ('Bob')"
            error "savepoint failure"

          -- Alice's insert should still be visible
          (rows, _) <- simpleQuery (txConn tx) "SELECT count(*) FROM users"
          case rows of
            [[Just n]] -> n `shouldBe` "1"
            _ -> expectationFailure $ "Expected 1, got: " <> show rows

        -- After commit, Alice should be in the table
        count <- withResource pool $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT count(*) FROM users"
          pure rows
        case count of
          [[Just n]] -> n `shouldBe` "1"
          _ -> expectationFailure $ "Expected 1, got: " <> show count

        withResource pool $ \conn -> dropSchema conn

    it "commits savepoint on success" $ do
      withTestPool $ \pool -> do
        withResource pool $ \conn -> do
          dropSchema conn
          createSchema conn

        withTransaction pool $ \tx -> do
          _ <- simpleQuery (txConn tx) "INSERT INTO users (name) VALUES ('Alice')"
          withSavepoint tx $ \_ -> do
            _ <- simpleQuery (txConn tx) "INSERT INTO users (name) VALUES ('Bob')"
            pure ()

          (rows, _) <- simpleQuery (txConn tx) "SELECT count(*) FROM users"
          case rows of
            [[Just n]] -> n `shouldBe` "2"
            _ -> expectationFailure $ "Expected 2, got: " <> show rows

        withResource pool $ \conn -> dropSchema conn

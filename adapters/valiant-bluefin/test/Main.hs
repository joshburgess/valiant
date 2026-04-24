module Main where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32)
import Data.Text (Text)
import Valiant (defaultPoolConfig, newPool, closePool)
import Valiant qualified
import Valiant.Bluefin (runValiantB, fetchAllB, fetchOneB, executeB)
import PgWire.Pool.Config (PoolConfig (..))
import System.Environment (lookupEnv)
import Test.Hspec
import TestSupport

main :: IO ()
main = hspec $ do
  describe "Valiant.Bluefin" $ do
    it "fetchAllB returns all rows" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        result <- runValiantB pool $ \db -> fetchAllB db stmtListAll ()
        closePool pool
        length result `shouldBe` 5

    it "fetchOneB returns Just for existing row" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        result <- runValiantB pool $ \db -> fetchOneB db stmtSelectOne 1
        closePool pool
        case result of
          Just (_, name, _) -> name `shouldBe` "Alice"
          Nothing -> expectationFailure "Expected a row"

    it "fetchOneB returns Nothing for missing row" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        pool <- mkPool
        result <- runValiantB pool $ \db -> fetchOneB db stmtSelectOne (-1)
        closePool pool
        result `shouldBe` (Nothing :: Maybe (Int32, Text, Maybe Text))

    it "executeB returns rows affected" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        let stmtDel = Valiant.mkStatement "DELETE FROM users_bluefin WHERE id = $1" [23] [] "<test>" :: Valiant.Statement Int32 ()
        n <- runValiantB pool $ \db -> executeB db stmtDel (1 :: Int32)
        closePool pool
        n `shouldBe` 1

    it "composes multiple operations" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        (users, mUser) <- runValiantB pool $ \db -> do
          us <- fetchAllB db stmtListAll ()
          mu <- fetchOneB db stmtSelectOne 1
          pure (us, mu)
        closePool pool
        length users `shouldBe` 5
        case mUser of
          Just (_, name, _) -> name `shouldBe` "Alice"
          Nothing -> expectationFailure "Expected a row"

mkPool :: IO Valiant.Pool
mkPool = do
  url <- requireDatabaseUrl'
  newPool defaultPoolConfig { poolConnString = url, poolSize = 2, poolAcquireTimeout = 5 }

requireDatabaseUrl' :: IO ByteString
requireDatabaseUrl' = do
  mUrl <- lookupEnv "DATABASE_URL"
  case mUrl of
    Just url -> pure (BS8.pack url)
    Nothing -> error "DATABASE_URL is not set."

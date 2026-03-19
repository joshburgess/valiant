module Main where

import Control.Carrier.Lift (runM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32)
import Data.Text (Text)
import Hsqlx (defaultPoolConfig, newPool, closePool)
import Hsqlx qualified
import Hsqlx.FusedEffects (fetchAllF, fetchOneF, executeF, runHsqlxPool)
import PgWire.Pool.Config (PoolConfig (..))
import System.Environment (lookupEnv)
import Test.Hspec
import TestSupport

main :: IO ()
main = hspec $ do
  describe "Hsqlx.FusedEffects" $ do
    it "fetchAllF returns all rows" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        result <- runM . runHsqlxPool pool $ fetchAllF stmtListAll ()
        closePool pool
        length result `shouldBe` 5

    it "fetchOneF returns Just for existing row" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        result <- runM . runHsqlxPool pool $ fetchOneF stmtSelectOne 1
        closePool pool
        case result of
          Just (_, name, _) -> name `shouldBe` "Alice"
          Nothing -> expectationFailure "Expected a row"

    it "fetchOneF returns Nothing for missing row" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        pool <- mkPool
        result <- runM . runHsqlxPool pool $ fetchOneF stmtSelectOne (-1)
        closePool pool
        result `shouldBe` (Nothing :: Maybe (Int32, Text, Maybe Text))

    it "executeF returns rows affected" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        let stmtDel = Hsqlx.mkStatement "DELETE FROM users WHERE id = $1" [23] [] "<test>" :: Hsqlx.Statement Int32 ()
        n <- runM . runHsqlxPool pool $ executeF stmtDel (1 :: Int32)
        closePool pool
        n `shouldBe` 1

    it "composes multiple operations" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        (users, mUser) <- runM . runHsqlxPool pool $ do
          us <- fetchAllF stmtListAll ()
          mu <- fetchOneF stmtSelectOne 1
          pure (us, mu)
        closePool pool
        length users `shouldBe` 5
        case mUser of
          Just (_, name, _) -> name `shouldBe` "Alice"
          Nothing -> expectationFailure "Expected a row"

mkPool :: IO Hsqlx.Pool
mkPool = do
  url <- requireDatabaseUrl'
  newPool defaultPoolConfig { poolConnString = url, poolSize = 2, poolAcquireTimeout = 5 }

requireDatabaseUrl' :: IO ByteString
requireDatabaseUrl' = do
  mUrl <- lookupEnv "DATABASE_URL"
  case mUrl of
    Just url -> pure (BS8.pack url)
    Nothing -> error "DATABASE_URL is not set."

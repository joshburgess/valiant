module Main where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32)
import Data.Text (Text)
import Effectful
import Valiant (defaultPoolConfig, newPool, closePool, txConn)
import Valiant qualified
import Valiant.Effectful
import PgWire.Pool.Config (PoolConfig (..))
import System.Environment (lookupEnv)
import Test.Hspec
import TestSupport

main :: IO ()
main = hspec $ do
  describe "Valiant.Effectful" $ do
    describe "runValiant" $ do
      it "fetchAllEff returns all rows" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          pool <- mkPool
          result <- runEff . runValiant pool $ fetchAllEff stmtListAll ()
          closePool pool
          length result `shouldBe` 5

      it "fetchOneEff returns Just for existing row" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          pool <- mkPool
          result <- runEff . runValiant pool $ fetchOneEff stmtSelectOne 1
          closePool pool
          case result of
            Just (_, name, _) -> name `shouldBe` "Alice"
            Nothing -> expectationFailure "Expected a row"

      it "fetchOneEff returns Nothing for missing row" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          pool <- mkPool
          result <- runEff . runValiant pool $ fetchOneEff stmtSelectOne (-1)
          closePool pool
          result `shouldBe` (Nothing :: Maybe (Int32, Text, Maybe Text))

      it "executeEff returns rows affected" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          pool <- mkPool
          let stmtDelete = Valiant.mkStatement "DELETE FROM users_effectful WHERE id = $1" [23] [] "<test>" :: Valiant.Statement Int32 ()
          n <- runEff . runValiant pool $ executeEff stmtDelete (1 :: Int32)
          closePool pool
          n `shouldBe` 1

      it "withTransactionEff commits" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          pool <- mkPool
          let stmtInsert = Valiant.mkStatement "INSERT INTO users_effectful (name, email) VALUES ($1, $2)" [25, 25] [] "<test>"
              stmtCount = Valiant.mkStatement "SELECT count(*)::int4 FROM users_effectful" [] ["count"] "<test>" :: Valiant.Statement () Int32
          _ <- runEff . runValiant pool $
            withTransactionEff $ \tx ->
              Valiant.execute (txConn tx) stmtInsert ("EffUser" :: Text, Just ("eff@test.com" :: Text))
          count <- runEff . runValiant pool $ fetchScalarEff stmtCount ()
          closePool pool
          count `shouldBe` 1

      it "composes multiple operations" $ do
        withTestConnection $ \conn -> withSchema conn $ do
          insertTestUsers conn
          pool <- mkPool
          (users, exists) <- runEff . runValiant pool $ do
            us <- fetchAllEff stmtListAll ()
            ex <- fetchExistsEff stmtSelectOne 1
            pure (us, ex)
          closePool pool
          length users `shouldBe` 5
          exists `shouldBe` True

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

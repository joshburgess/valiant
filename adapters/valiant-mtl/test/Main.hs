module Main where

import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32)
import Data.Text (Text)
import Valiant (defaultPoolConfig, newPool, closePool)
import Valiant qualified
import Valiant.Monad (runValiant, fetchAllM, fetchOneM, executeM, fetchScalarM)
import Valiant.Mtl (HasPool (..), fetchAllMtl, fetchOneMtl, fetchScalarMtl, withTransactionMtl)
import PgWire.Pool.Config (PoolConfig (..))
import System.Environment (lookupEnv)
import Test.Hspec
import TestSupport

-- HasPool instance for ReaderT Pool IO
instance HasPool (ReaderT Valiant.Pool IO) where
  getPool = ask

main :: IO ()
main = hspec $ do
  describe "Valiant.Monad (ReaderT Pool IO)" $ do
    it "fetchAllM returns all rows" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        result <- runValiant pool $ fetchAllM stmtListAll ()
        closePool pool
        length result `shouldBe` 5

    it "fetchOneM returns Just for existing row" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        result <- runValiant pool $ fetchOneM stmtSelectOne 1
        closePool pool
        case result of
          Just (_, name, _) -> name `shouldBe` "Alice"
          Nothing -> expectationFailure "Expected a row"

    it "executeM returns rows affected" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        let stmtDel = Valiant.mkStatement "DELETE FROM users WHERE id = $1" [23] [] "<test>" :: Valiant.Statement Int32 ()
        n <- runValiant pool $ executeM stmtDel (1 :: Int32)
        closePool pool
        n `shouldBe` 1

    it "composes multiple operations" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        let stmtCount = Valiant.mkStatement "SELECT count(*)::int4 FROM users" [] ["count"] "<test>" :: Valiant.Statement () Int32
        (users, count) <- runValiant pool $ do
          us <- fetchAllM stmtListAll ()
          c <- fetchScalarM stmtCount ()
          pure (us, c)
        closePool pool
        length users `shouldBe` 5
        count `shouldBe` 5

  describe "Valiant.Mtl (HasPool)" $ do
    it "fetchAllMtl works with ReaderT Pool IO" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        insertTestUsers conn
        pool <- mkPool
        result <- runReaderT (fetchAllMtl stmtListAll ()) pool
        closePool pool
        length result `shouldBe` 5

    it "fetchOneMtl returns Nothing for missing row" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        pool <- mkPool
        result <- runReaderT (fetchOneMtl stmtSelectOne (-1)) pool
        closePool pool
        result `shouldBe` (Nothing :: Maybe (Int32, Text, Maybe Text))

    it "withTransactionMtl commits" $ do
      withTestConnection $ \conn -> withSchema conn $ do
        pool <- mkPool
        let stmtInsert = Valiant.mkStatement "INSERT INTO users (name, email) VALUES ($1, $2)" [25, 25] [] "<test>"
            stmtCount = Valiant.mkStatement "SELECT count(*)::int4 FROM users" [] ["count"] "<test>" :: Valiant.Statement () Int32
        _ <- runReaderT (withTransactionMtl $ \tx ->
          Valiant.execute (Valiant.txConn tx) stmtInsert ("MtlUser" :: Text, Just ("mtl@test.com" :: Text))) pool
        count <- runReaderT (fetchScalarMtl stmtCount ()) pool
        closePool pool
        count `shouldBe` 1

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

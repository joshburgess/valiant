module Valiant.CLI.ErrorSpec (spec) where

import Data.Text qualified as T
import PgWire.Protocol.Oid (Oid (..))
import Valiant.CLI.Error
import Test.Hspec

spec :: Spec
spec = do
  describe "renderError" $ do
    it "produces non-empty text for ErrNoDatabaseUrl" $
      renderError ErrNoDatabaseUrl `shouldNotBe` ""

    it "produces non-empty text for ErrConnectionFailed" $
      renderError (ErrConnectionFailed "timeout") `shouldNotBe` ""

    it "produces non-empty text for ErrSqlDirNotFound" $
      renderError (ErrSqlDirNotFound "sql/") `shouldNotBe` ""

    it "produces non-empty text for ErrNoSqlFiles" $
      renderError (ErrNoSqlFiles "sql/") `shouldNotBe` ""

    it "produces non-empty text for ErrDescribeFailed" $
      renderError (ErrDescribeFailed "test.sql" pgErr) `shouldNotBe` ""

    it "produces non-empty text for ErrUnknownOid" $
      renderError (ErrUnknownOid "test.sql" "col" (Oid 600)) `shouldNotBe` ""

    it "produces non-empty text for ErrCacheReadFailed" $
      renderError (ErrCacheReadFailed "cache.json" "parse error") `shouldNotBe` ""

    it "produces non-empty text for ErrStaleCacheFile" $
      renderError (ErrStaleCacheFile "test.sql" "aaa" "bbb") `shouldNotBe` ""

    it "produces non-empty text for ErrMissingCacheFile" $
      renderError (ErrMissingCacheFile "test.sql") `shouldNotBe` ""

    it "ErrNoDatabaseUrl mentions DATABASE_URL" $
      renderError ErrNoDatabaseUrl `shouldSatisfy` T.isInfixOf "DATABASE_URL"

    it "ErrSqlDirNotFound includes the directory path" $
      renderError (ErrSqlDirNotFound "my/sql/dir")
        `shouldSatisfy` T.isInfixOf "my/sql/dir"

    it "ErrNoSqlFiles includes the directory path" $
      renderError (ErrNoSqlFiles "some/dir")
        `shouldSatisfy` T.isInfixOf "some/dir"

    it "ErrStaleCacheFile includes both hashes" $ do
      let rendered = renderError (ErrStaleCacheFile "q.sql" "abc123" "def456")
      rendered `shouldSatisfy` T.isInfixOf "abc123"
      rendered `shouldSatisfy` T.isInfixOf "def456"

    it "ErrMissingCacheFile mentions valiant prepare" $
      renderError (ErrMissingCacheFile "q.sql")
        `shouldSatisfy` T.isInfixOf "valiant prepare"

    it "ErrDescribeFailed includes the PG error message" $
      renderError (ErrDescribeFailed "q.sql" pgErr)
        `shouldSatisfy` T.isInfixOf "column \"emal\" does not exist"

pgErr :: PgErrorDetail
pgErr =
  PgErrorDetail
    { pgMessage = "column \"emal\" does not exist"
    , pgDetail = Nothing
    , pgHint = Just "Perhaps you meant \"email\"."
    , pgPosition = Nothing
    }

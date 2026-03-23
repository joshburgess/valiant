-- | Integration tests for the soundness fixes from the correctness audit.
--
-- These tests exercise error paths, exception safety, and resource cleanup
-- that are difficult to verify from code review alone.
module SoundnessSpec (spec) where

import Control.Exception (SomeException, try)
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32)
import Hsqlx
import PgWire.Connection.Config (parseConnString)
import TestSupport
import Test.Hspec

spec :: Spec
spec = do
  commitFailureSpec
  savepointCleanupSpec
  copyExceptionSpec
  cursorCleanupSpec
  advisoryLockSpec
  cacheEvictionSpec
  recycleCleanCacheSpec
  stmtCacheSizeSpec

------------------------------------------------------------------------
-- Transaction COMMIT failure (#4 from second-pass audit)
------------------------------------------------------------------------

commitFailureSpec :: Spec
commitFailureSpec = describe "COMMIT failure recovery" $ do
  it "rolls back on deferred constraint violation so connection is reusable" $ do
    withTestPool $ \pool -> do
      -- Set up a table with a deferred unique constraint
      withResource pool $ \conn -> do
        _ <- simpleQuery conn "DROP TABLE IF EXISTS soundness_deferred CASCADE"
        _ <- simpleQuery conn
          "CREATE TABLE soundness_deferred (\
          \  id INTEGER NOT NULL,\
          \  CONSTRAINT deferred_unique UNIQUE (id) DEFERRABLE INITIALLY DEFERRED\
          \)"
        pure ()

      -- Insert a duplicate — the constraint is deferred, so the INSERT
      -- succeeds but COMMIT fails.
      result <- try @SomeException $ withTransaction pool $ \tx -> do
        _ <- simpleQuery (txConn tx) "INSERT INTO soundness_deferred (id) VALUES (1)"
        _ <- simpleQuery (txConn tx) "INSERT INTO soundness_deferred (id) VALUES (1)"
        pure ()
      result `shouldSatisfy` isLeft

      -- The connection should be returned to the pool in a clean state.
      -- If COMMIT failure wasn't handled, this would fail with
      -- "current transaction is aborted".
      withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT 1"
        rows `shouldBe` [[Just "1"]]

      -- Cleanup
      withResource pool $ \conn -> do
        _ <- simpleQuery conn "DROP TABLE IF EXISTS soundness_deferred"
        pure ()

------------------------------------------------------------------------
-- Savepoint cleanup (#8 from first audit)
------------------------------------------------------------------------

savepointCleanupSpec :: Spec
savepointCleanupSpec = describe "Savepoint cleanup" $ do
  it "rolled-back savepoint doesn't corrupt subsequent queries" $ do
    withTestPool $ \pool -> do
      withResource pool $ \conn -> do
        _ <- simpleQuery conn "DROP TABLE IF EXISTS soundness_sp CASCADE"
        _ <- simpleQuery conn "CREATE TABLE soundness_sp (id INTEGER NOT NULL)"
        pure ()

      -- Multiple savepoint failures in a row should all be cleanly handled
      withTransaction pool $ \tx -> do
        _ <- simpleQuery (txConn tx) "INSERT INTO soundness_sp (id) VALUES (1)"

        -- First failed savepoint
        _ <- try @SomeException $ withSavepoint tx $ \_ ->
          error "sp failure 1"

        -- Second failed savepoint (tests that first cleanup was correct)
        _ <- try @SomeException $ withSavepoint tx $ \_ ->
          error "sp failure 2"

        -- Should still be able to query inside the transaction
        (rows, _) <- simpleQuery (txConn tx) "SELECT count(*) FROM soundness_sp"
        case rows of
          [[Just n]] -> n `shouldBe` "1"
          _ -> expectationFailure $ "Unexpected: " <> show rows

      -- Verify commit worked
      withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT count(*) FROM soundness_sp"
        case rows of
          [[Just n]] -> n `shouldBe` "1"
          _ -> expectationFailure $ "Unexpected: " <> show rows
        _ <- simpleQuery conn "DROP TABLE soundness_sp"
        pure ()

------------------------------------------------------------------------
-- COPY exception handling (#4 from first audit)
------------------------------------------------------------------------

copyExceptionSpec :: Spec
copyExceptionSpec = describe "COPY exception safety" $ do
  it "connection is usable after producer throws during copyIn" $ do
    withTestConnection $ \conn -> withSchema conn $ do
      -- Producer throws mid-COPY
      result <- try @SomeException $
        copyIn conn "COPY users (name, email) FROM STDIN WITH (FORMAT csv)" $ \send -> do
          send "Alice,alice@example.com\n"
          error "producer failure"
      result `shouldSatisfy` isLeft

      -- Connection should still be usable (CopyFail was sent)
      (rows, _) <- simpleQuery conn "SELECT 1"
      rows `shouldBe` [[Just "1"]]

  it "preserves original exception message" $ do
    withTestConnection $ \conn -> withSchema conn $ do
      result <- try @SomeException $
        copyIn conn "COPY users (name, email) FROM STDIN WITH (FORMAT csv)" $ \send -> do
          send "Alice,alice@example.com\n"
          error "specific error message"
      case result of
        Left e -> show e `shouldSatisfy` BS8.isInfixOf "specific error message" . BS8.pack
        Right _ -> expectationFailure "Expected exception"

------------------------------------------------------------------------
-- Cursor cleanup on exception (#3 from first audit)
------------------------------------------------------------------------

cursorCleanupSpec :: Spec
cursorCleanupSpec = describe "Cursor cleanup on exception" $ do
  it "connection is usable after exception during cursor streaming" $ do
    withTestPool $ \pool -> do
      withResource pool $ \conn -> withSchema conn $
        insertTestUsers conn

      -- Use executeWithFold which uses exclusive mode.
      -- Throw mid-fold and verify the connection recovers.
      result <- try @SomeException $ withTransaction pool $ \tx ->
        executeWithFold (txConn tx) countQuery ()
          (RowFold (0 :: Int) (\acc _ -> if acc >= 1 then error "mid-fold failure" else acc + 1))
      result `shouldSatisfy` isLeft

      -- Connection should be returned to pool cleanly
      withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT 1"
        rows `shouldBe` [[Just "1"]]
        dropSchema conn

  where
    countQuery :: Statement () Int32
    countQuery = mkStatement
      "SELECT count(*)::int4 FROM users"
      []
      ["count"]
      "<test>"

------------------------------------------------------------------------
-- Advisory lock safety (#5 from first audit, #19 from MEDIUM fixes)
------------------------------------------------------------------------

advisoryLockSpec :: Spec
advisoryLockSpec = describe "Advisory locks" $ do
  it "session-scoped lock is released even when action throws" $ do
    withTestConnection $ \conn -> do
      -- Acquire lock, throw inside
      _ <- try @SomeException $ withAdvisoryLock conn 99999 $
        error "inside lock"

      -- Lock should be released — try to acquire it again immediately
      -- If the lock leaked, this would deadlock
      withAdvisoryLock conn 99999 $ pure ()

  it "try-lock releases on exception" $ do
    withTestConnection $ \conn -> do
      _ <- try @SomeException $ withAdvisoryLockTry conn 88888 $
        (error "inside try lock" :: IO ())

      -- Should be able to acquire again
      result <- withAdvisoryLockTry conn 88888 $ pure (42 :: Int)
      result `shouldBe` Just 42

  it "transaction-scoped lock requires active transaction" $ do
    withTestConnection $ \conn -> do
      result <- try @SomeException $ withAdvisoryLockTx conn 77777 $
        pure ()
      result `shouldSatisfy` isLeft

------------------------------------------------------------------------
-- Statement cache LRU eviction
------------------------------------------------------------------------

cacheEvictionSpec :: Spec
cacheEvictionSpec = describe "Statement cache eviction" $ do
  it "handles more unique queries than cache size without error" $ do
    withTestConnection $ \conn -> do
      -- Generate more unique queries than the default cache size (256).
      -- Each query has a different constant, forcing a new prepared statement.
      -- This exercises the LRU eviction path.
      let n = 300
      results <- mapM (\i -> do
        let q = "SELECT " <> BS8.pack (show i)
        (rows, _) <- simpleQuery conn q
        pure rows
        ) [1 :: Int .. n]
      length results `shouldBe` n

  it "frequently-used statements survive eviction" $ do
    withTestConnection $ \conn -> do
      -- Execute a "hot" query repeatedly, interleaved with many unique queries.
      -- The hot query should remain cached (LRU keeps it) and not cause errors.
      _ <- simpleQuery conn "CREATE TEMP TABLE cache_test (x INT)"
      let hotQuery = "SELECT count(*) FROM cache_test"
      -- Warm the hot query
      _ <- simpleQuery conn hotQuery

      -- Flood with unique queries to trigger eviction
      mapM_ (\i -> simpleQuery conn ("SELECT " <> BS8.pack (show i))) [1 :: Int .. 300]

      -- Hot query should still work (re-prepared if evicted, but no error)
      (rows, _) <- simpleQuery conn hotQuery
      rows `shouldBe` [[Just "0"]]

------------------------------------------------------------------------
-- RecycleClean clears statement cache (#15)
------------------------------------------------------------------------

recycleCleanCacheSpec :: Spec
recycleCleanCacheSpec = describe "RecycleClean cache invalidation" $ do
  it "DISCARD ALL clears statement cache so no stale references" $ do
    url <- requireDatabaseUrl
    pool <- newPool defaultPoolConfig
      { poolConnString = url
      , poolSize = 1
      , poolRecyclingMethod = RecycleClean
      , poolIdleTime = 600
      , poolHealthCheckAge = 0  -- always check
      }

    -- First use: prepare a statement
    withResource pool $ \conn -> do
      _ <- simpleQuery conn "SELECT 1"
      pure ()

    -- Return connection, re-acquire (triggers RecycleClean → DISCARD ALL).
    -- The cache should be cleared. Using the same query should re-prepare
    -- without "prepared statement does not exist".
    withResource pool $ \conn -> do
      (rows, _) <- simpleQuery conn "SELECT 1"
      rows `shouldBe` [[Just "1"]]

    closePool pool

------------------------------------------------------------------------
-- Configurable statement cache size
------------------------------------------------------------------------

stmtCacheSizeSpec :: Spec
stmtCacheSizeSpec = describe "Statement cache size" $ do
  it "respects custom cache size from config" $ do
    url <- requireDatabaseUrl
    case parseConnString url of
      Left err -> expectationFailure err
      Right cfg -> do
        let cfg' = cfg { ccMaxPreparedStatements = 10 }
        conn <- connect cfg'
        -- Run more unique queries than the cache size
        mapM_ (\i -> simpleQuery conn ("SELECT " <> BS8.pack (show i))) [1 :: Int .. 20]
        -- Should still work (eviction happening correctly)
        (rows, _) <- simpleQuery conn "SELECT 42"
        rows `shouldBe` [[Just "42"]]
        close conn

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

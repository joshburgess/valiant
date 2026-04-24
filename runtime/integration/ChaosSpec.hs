-- | Chaos tests: verify correct behavior under concurrent stress and
-- connection failure scenarios.
module ChaosSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (SomeException, try)
import Data.ByteString.Char8 qualified as BS8
import Valiant
import TestSupport
import Test.Hspec

spec :: Spec
spec = do
  poolStressSpec
  terminationRecoverySpec

------------------------------------------------------------------------
-- Pool stress tests
------------------------------------------------------------------------

poolStressSpec :: Spec
poolStressSpec = describe "Pool stress" $ do
  it "handles 100 rapid concurrent acquire/release without corruption" $ do
    withTestPool $ \pool -> do
      mapConcurrently_
        (\_ -> do
          _ <- try @SomeException $ withResource pool $ \conn ->
            simpleQuery conn "SELECT 1"
          pure ()
        )
        [1 :: Int .. 100]

      -- Pool should still be functional
      withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT 1"
        rows `shouldBe` [[Just "1"]]

  it "statement cache handles 50 concurrent distinct queries" $ do
    withTestPool $ \pool -> do
      mapConcurrently_
        (\i -> withResource pool $ \conn -> do
          let q = "SELECT " <> BS8.pack (show i)
          _ <- simpleQuery conn q
          pure ()
        )
        [1 :: Int .. 50]

      withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT 1"
        rows `shouldBe` [[Just "1"]]

  it "mixed queries and transactions under contention" $ do
    withTestPool $ \pool -> do
      withResource pool $ \conn -> do
        _ <- simpleQuery conn "DROP TABLE IF EXISTS chaos_mix CASCADE"
        _ <- simpleQuery conn "CREATE TABLE chaos_mix (id SERIAL PRIMARY KEY, val INT NOT NULL)"
        pure ()

      -- Mix of reads and writes across threads
      mapConcurrently_
        (\i -> do
          if even i
            then do
              -- Write path
              _ <- try @SomeException $ withTransaction pool $ \tx ->
                simpleQuery (txConn tx) ("INSERT INTO chaos_mix (val) VALUES (" <> BS8.pack (show i) <> ")")
              pure ()
            else do
              -- Read path
              _ <- try @SomeException $ withResource pool $ \conn ->
                simpleQuery conn "SELECT count(*) FROM chaos_mix"
              pure ()
        )
        [1 :: Int .. 40]

      withResource pool $ \conn -> do
        (rows, _) <- simpleQuery conn "SELECT count(*) FROM chaos_mix"
        case rows of
          [[Just n]] -> (read (BS8.unpack n) :: Int) `shouldSatisfy` (> 0)
          _ -> expectationFailure $ "Unexpected: " <> show rows
        _ <- simpleQuery conn "DROP TABLE chaos_mix"
        pure ()

------------------------------------------------------------------------
-- Connection termination recovery
------------------------------------------------------------------------

terminationRecoverySpec :: Spec
terminationRecoverySpec = describe "Termination recovery" $ do
  it "pool recovers when idle connection is terminated by server" $ do
    url <- requireDatabaseUrl
    pool <- newPool defaultPoolConfig
      { poolConnString = url
      , poolSize = 1
      , poolAcquireTimeout = 5
      , poolRecyclingMethod = RecycleVerified
      , poolHealthCheckAge = 0  -- always verify
      }

    -- Populate the pool with one connection
    withResource pool $ \conn -> do
      _ <- simpleQuery conn "SELECT 1"
      pure ()

    -- Terminate the idle connection from outside.
    -- Use a fresh connection (not from the pool) to issue the kill.
    withTestConnection $ \killer -> do
      -- Find PIDs of all connections from our test user
      (rows, _) <- simpleQuery killer
        "SELECT pid FROM pg_stat_activity WHERE usename = 'valiant_test' AND pid != pg_backend_pid() AND state = 'idle'"
      mapM_ (\row -> case row of
        [Just pid] -> do
          _ <- simpleQuery killer ("SELECT pg_terminate_backend(" <> pid <> ")")
          pure ()
        _ -> pure ()
        ) rows

    -- Wait for termination
    threadDelay 300000

    -- Pool should detect dead connection (RecycleVerified + healthCheckAge=0)
    -- and create a fresh one. May need a retry since first acquire gets the
    -- dead connection which fails health check.
    let tryQuery = withResource pool $ \conn -> do
          (rows, _) <- simpleQuery conn "SELECT 1"
          pure rows
    result1 <- try @SomeException tryQuery
    result2 <- try @SomeException tryQuery
    let succeeded = case (result1, result2) of
          (Right [[Just "1"]], _) -> True
          (_, Right [[Just "1"]]) -> True
          _ -> False
    succeeded `shouldBe` True

    closePool pool

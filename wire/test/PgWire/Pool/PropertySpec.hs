module PgWire.Pool.PropertySpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (try)
import Data.ByteString.Char8 qualified as BS8
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PgWire.Connection (simpleQuery)
import PgWire.Error (ValiantError (..))
import PgWire.MockServer (defaultMockConfig, withMockServer)
import PgWire.Pool (Pool, PoolStats (..), closePool, newPool, poolIsAlive, poolStats, resize, withResource)
import PgWire.Pool.Config (PoolConfig (..), defaultPoolConfig)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

-- | Create a pool backed by the mock server.
withMockPool :: Int -> (Pool -> IO a) -> IO a
withMockPool size action =
  withMockServer defaultMockConfig $ \port -> do
    let connStr = "postgres://testuser:testpass@127.0.0.1:" <> BS8.pack (show port) <> "/testdb"
    pool <- newPool defaultPoolConfig
      { poolConnString = connStr
      , poolSize = size
      , poolAcquireTimeout = 5
      }
    result <- action pool
    closePool pool
    pure result

spec :: Spec
spec = do
  describe "Pool invariants" $ do
    it "idle + inUse never exceeds maxSize" $ hedgehog $ do
      size <- forAll (Gen.int (Range.linear 1 10))
      nOps <- forAll (Gen.int (Range.linear 1 20))
      result <- evalIO $ withMockPool size $ \pool -> do
        -- Do several acquire/release cycles
        mapM_ (\_ -> withResource pool $ \conn ->
          simpleQuery conn "SELECT 1") [1 .. nOps]
        stats <- poolStats pool
        pure (psIdle stats + psInUse stats <= psMaxSize stats)
      assert result

    it "all connections returned after withResource" $ hedgehog $ do
      size <- forAll (Gen.int (Range.linear 1 8))
      nOps <- forAll (Gen.int (Range.linear 1 15))
      stats <- evalIO $ withMockPool size $ \pool -> do
        mapM_ (\_ -> withResource pool $ \conn ->
          simpleQuery conn "SELECT 1") [1 .. nOps]
        poolStats pool
      psInUse stats === 0

    it "concurrent acquire/release maintains invariant" $ hedgehog $ do
      size <- forAll (Gen.int (Range.linear 2 8))
      nThreads <- forAll (Gen.int (Range.linear 2 10))
      stats <- evalIO $ withMockPool size $ \pool -> do
        _ <- mapConcurrently (\_ ->
          withResource pool $ \conn ->
            simpleQuery conn "SELECT 1") [1 .. nThreads]
        poolStats pool
      -- After all threads finish, no connections should be in use
      psInUse stats === 0
      -- idle + inUse should not exceed max
      assert (psIdle stats <= size)

    it "totalCreated >= totalDestroyed" $ hedgehog $ do
      size <- forAll (Gen.int (Range.linear 1 5))
      nOps <- forAll (Gen.int (Range.linear 1 10))
      stats <- evalIO $ withMockPool size $ \pool -> do
        mapM_ (\_ -> withResource pool $ \conn ->
          simpleQuery conn "SELECT 1") [1 .. nOps]
        poolStats pool
      assert (psTotalCreated stats >= psTotalDestroyed stats)

    it "closePool makes pool not alive" $ hedgehog $ do
      size <- forAll (Gen.int (Range.linear 1 5))
      alive <- evalIO $ withMockServer defaultMockConfig $ \port -> do
        let connStr = "postgres://testuser:testpass@127.0.0.1:" <> BS8.pack (show port) <> "/testdb"
        pool <- newPool defaultPoolConfig
          { poolConnString = connStr
          , poolSize = size
          , poolAcquireTimeout = 5
          }
        closePool pool
        poolIsAlive pool
      assert (not alive)

    it "closePool rejects new acquires" $ hedgehog $ do
      size <- forAll (Gen.int (Range.linear 1 5))
      gotError <- evalIO $ withMockServer defaultMockConfig $ \port -> do
        let connStr = "postgres://testuser:testpass@127.0.0.1:" <> BS8.pack (show port) <> "/testdb"
        pool <- newPool defaultPoolConfig
          { poolConnString = connStr
          , poolSize = size
          , poolAcquireTimeout = 5
          }
        closePool pool
        result <- try (withResource pool $ \_ -> pure ())
        pure $ case result of
          Left PoolClosed -> True
          Left _ -> False
          Right _ -> False
      assert gotError

    it "resize changes maxSize" $ hedgehog $ do
      initial <- forAll (Gen.int (Range.linear 2 10))
      newSize <- forAll (Gen.int (Range.linear 1 20))
      stats <- evalIO $ withMockPool initial $ \pool -> do
        resize pool newSize
        poolStats pool
      psMaxSize stats === newSize

    it "pool stats are consistent" $ hedgehog $ do
      size <- forAll (Gen.int (Range.linear 1 8))
      nOps <- forAll (Gen.int (Range.linear 0 10))
      stats <- evalIO $ withMockPool size $ \pool -> do
        mapM_ (\_ -> withResource pool $ \conn ->
          simpleQuery conn "SELECT 1") [1 .. nOps]
        poolStats pool
      -- Basic consistency checks
      assert (psIdle stats >= 0)
      assert (psInUse stats >= 0)
      assert (psWaiters stats >= 0)
      assert (psMaxSize stats > 0)
      assert (psTotalCreated stats >= 0)
      assert (psTotalDestroyed stats >= 0)
      assert (psTotalTimeouts stats >= 0)

module PgWire.Pool.NoThunksSpec (spec) where

import NoThunks.Class (noThunks)
import PgWire.Pool (PoolStats (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "NoThunks" $ do
    it "PoolStats has no thunks when fully evaluated" $ do
      let stats = PoolStats
            { psIdle = 3
            , psInUse = 5
            , psWaiters = 0
            , psMaxSize = 10
            , psTotalCreated = 20
            , psTotalDestroyed = 12
            , psTotalTimeouts = 1
            }
      result <- noThunks [] stats
      case result of
        Nothing -> pure ()
        Just thunkInfo -> expectationFailure $ "Found thunk: " <> show thunkInfo

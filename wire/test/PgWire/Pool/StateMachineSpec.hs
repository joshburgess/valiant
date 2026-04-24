-- | Lightweight state-machine tests for the connection pool.
--
-- Existing 'PgWire.Pool.PropertySpec' tests cover individual scenarios
-- (a burst of acquires, a single resize, a single close). This module
-- generates arbitrary sequences of Burst/Resize/Close and checks the
-- model-vs-real invariants after every single step. The model is
-- hand-rolled rather than using 'Hedgehog.Internal.State' so the test
-- drops straight into the existing hspec-hedgehog runner.
module PgWire.Pool.StateMachineSpec (spec) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (try)
import Data.ByteString.Char8 qualified as BS8
import Hedgehog hiding (Command)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PgWire.Connection (simpleQuery)
import PgWire.Error (PgWireError (..))
import PgWire.MockServer (defaultMockConfig, withMockServer)
import PgWire.Pool
  ( Pool
  , PoolStats (..)
  , closePool
  , newPool
  , poolIsAlive
  , poolStats
  , resize
  , withResource
  )
import PgWire.Pool.Config (PoolConfig (..), defaultPoolConfig)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

data Command
  = CmdBurst !Int
  | CmdResize !Int
  | CmdClose
  deriving (Show)

data Model = Model
  { mMaxSize :: !Int
  , mClosed :: !Bool
  }
  deriving (Show)

initialModel :: Int -> Model
initialModel size = Model {mMaxSize = size, mClosed = False}

stepModel :: Model -> Command -> Model
stepModel m cmd = case cmd of
  CmdBurst _ -> m
  CmdResize n -> m {mMaxSize = n}
  CmdClose -> m {mClosed = True}

-- | Burst issues n concurrent acquire/release cycles. Errors are
-- swallowed: if the pool is closed or the acquire times out, the
-- model already reflects that and the invariant pass after the
-- command settles is what matters.
runCommand :: Pool -> Command -> IO ()
runCommand pool cmd = case cmd of
  CmdBurst n ->
    mapConcurrently_
      ( \_ -> do
          _ <-
            try @PgWireError $
              withResource pool $ \conn -> simpleQuery conn "SELECT 1"
          pure ()
      )
      [1 .. n]
  CmdResize n -> resize pool n
  CmdClose -> closePool pool

genCommand :: Gen Command
genCommand =
  Gen.frequency
    [ (6, CmdBurst <$> Gen.int (Range.linear 1 6))
    , (2, CmdResize <$> Gen.int (Range.linear 1 8))
    , (1, pure CmdClose)
    ]

-- | Generate a program with at most one Close so we don't burn cycles
-- on post-close no-ops. Close may appear at any position or not at all.
genCommands :: Gen [Command]
genCommands = do
  len <- Gen.int (Range.linear 1 12)
  cmds <- Gen.list (Range.singleton len) (Gen.filterT (not . isClose) genCommand)
  includeClose <- Gen.bool
  if includeClose
    then do
      pos <- Gen.int (Range.linear 0 (length cmds))
      let (hd, tl) = splitAt pos cmds
      pure (hd <> [CmdClose] <> tl)
    else pure cmds
  where
    isClose CmdClose = True
    isClose _ = False

-- | A per-step observation collected while running the program, then
-- re-checked inside the Hedgehog property.
data Step = Step
  { stCmd :: !Command
  , stModel :: !Model
  , stStats :: !PoolStats
  , stAlive :: !Bool
  , stClosedProbe :: !(Maybe (Either PgWireError ()))
  -- ^ If the model says closed, probe the real pool by trying to
  -- acquire a connection. Expected: Left PoolClosed.
  }
  deriving (Show)

runProgram :: Pool -> Int -> [Command] -> IO [Step]
runProgram pool initialSize = go (initialModel initialSize) []
  where
    go _ acc [] = pure (reverse acc)
    go m acc (c : cs) = do
      runCommand pool c
      let m' = stepModel m c
      stats <- poolStats pool
      alive <- poolIsAlive pool
      probe <-
        if mClosed m'
          then do
            r <- try @PgWireError (withResource pool $ \_ -> pure ())
            pure (Just r)
          else pure Nothing
      let step = Step c m' stats alive probe
      go m' (step : acc) cs

withMockPool :: Int -> (Pool -> IO a) -> IO a
withMockPool size action =
  withMockServer defaultMockConfig $ \port -> do
    let connStr = "postgres://testuser:testpass@127.0.0.1:" <> BS8.pack (show port) <> "/testdb"
    pool <-
      newPool
        defaultPoolConfig
          { poolConnString = connStr
          , poolSize = size
          , poolAcquireTimeout = 2
          }
    r <- action pool
    closePool pool -- idempotent; safe even if the program closed it
    pure r

checkStep :: (MonadTest m) => Step -> m ()
checkStep Step {stCmd, stModel, stStats, stAlive, stClosedProbe} = do
  annotateShow stCmd
  annotateShow stModel
  annotateShow stStats
  stAlive === not (mClosed stModel)
  psMaxSize stStats === mMaxSize stModel
  -- After a command has settled, nothing should remain in flight.
  psInUse stStats === 0
  -- Idle never exceeds the current cap (resize-down drops excess idle
  -- eagerly, so even a just-executed shrink should satisfy this).
  assert (psIdle stStats <= mMaxSize stModel)
  assert (psIdle stStats >= 0)
  assert (psWaiters stStats >= 0)
  assert (psTotalCreated stStats >= 0)
  assert (psTotalDestroyed stStats >= 0)
  assert (psTotalTimeouts stStats >= 0)
  -- Destroy count can never exceed create count.
  assert (psTotalCreated stStats >= psTotalDestroyed stStats)
  case stClosedProbe of
    Nothing -> pure ()
    Just (Left PoolClosed) -> pure ()
    Just other -> do
      annotateShow other
      failure

spec :: Spec
spec = do
  describe "Pool state machine" $ do
    it "model-vs-pool invariants hold after every command" $ hedgehog $ do
      initialSize <- forAll (Gen.int (Range.linear 1 6))
      program <- forAll genCommands
      steps <- evalIO $ withMockPool initialSize $ \pool ->
        runProgram pool initialSize program
      mapM_ checkStep steps

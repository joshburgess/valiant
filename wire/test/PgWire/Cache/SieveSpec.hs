{-# LANGUAGE LambdaCase #-}

module PgWire.Cache.SieveSpec (spec) where

import Prelude hiding (lookup)

import Control.Monad (foldM, forM_, when)
import Data.IORef
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PgWire.Cache.Sieve (SieveCache, capacity, clear, delete, foldEntries, insert, lookup, newSieveCache, size)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

------------------------------------------------------------------------
-- Reference model
------------------------------------------------------------------------

-- | Pure model of the SIEVE cache's externally-observable state.
--
-- We do not model SIEVE's eviction order: it's an implementation
-- detail driven by the hand pointer and the visited bits. Instead,
-- when an insert evicts, we accept whatever key SIEVE chose and
-- reflect that in the model.
data Model k v = Model
  { mCap :: !Int
  , mMap :: !(Map k v)
  } deriving (Show)

newModel :: Int -> Model k v
newModel cap = Model cap Map.empty

modelSize :: Model k v -> Int
modelSize = Map.size . mMap

modelLookup :: (Ord k) => k -> Model k v -> Maybe v
modelLookup k = Map.lookup k . mMap

-- | Apply an insert in the model, given what SIEVE reported as evicted.
modelInsert :: (Ord k) => k -> v -> Maybe (k, v) -> Model k v -> Model k v
modelInsert k v evicted m =
  let m' = case evicted of
        Nothing -> m
        Just (ek, _) -> m { mMap = Map.delete ek (mMap m) }
   in m' { mMap = Map.insert k v (mMap m') }

modelDelete :: (Ord k) => k -> Model k v -> Model k v
modelDelete k m = m { mMap = Map.delete k (mMap m) }

------------------------------------------------------------------------
-- Operation generators
------------------------------------------------------------------------

data Op k v
  = OpInsert !k !v
  | OpLookup !k
  | OpDelete !k
  deriving (Show)

-- | Generate ops drawn from a small key universe so that hits, evictions,
-- and updates all exercise the cache. Operation mix is intentionally
-- skewed toward insert/lookup since that's the prepared-statement workload.
genOp :: Range.Range Int -> Gen (Op Int Int)
genOp keyRange = Gen.frequency
  [ (5, OpInsert <$> Gen.int keyRange <*> Gen.int (Range.linear 0 1000))
  , (4, OpLookup <$> Gen.int keyRange)
  , (1, OpDelete <$> Gen.int keyRange)
  ]

------------------------------------------------------------------------
-- spec
------------------------------------------------------------------------

spec :: Spec
spec = do
  -- Original unit tests, preserved.
  describe "basic operations" $ do
    it "empty cache has size 0" $ do
      sc <- newSieveCache @Int @Int 10
      sz <- size sc
      sz `shouldBe` 0

    it "insert increases size" $ do
      sc <- newSieveCache @Int @String 5
      _ <- insert sc 1 "a"
      sz <- size sc
      sz `shouldBe` 1

    it "lookup finds inserted entry" $ do
      sc <- newSieveCache @Int @String 5
      _ <- insert sc 1 "hello"
      val <- lookup sc 1
      val `shouldBe` Just "hello"

    it "lookup returns Nothing for missing key" $ do
      sc <- newSieveCache @Int @String 5
      _ <- insert sc 1 "hello"
      val <- lookup sc 2
      val `shouldBe` Nothing

    it "delete removes entry" $ do
      sc <- newSieveCache @Int @String 5
      _ <- insert sc 1 "hello"
      delete sc 1
      val <- lookup sc 1
      val `shouldBe` Nothing
      sz <- size sc
      sz `shouldBe` 0

    it "delete on missing key is a no-op" $ do
      sc <- newSieveCache @Int @String 5
      _ <- insert sc 1 "hello"
      delete sc 99
      sz <- size sc
      sz `shouldBe` 1

    it "insert returns Nothing when under capacity" $ do
      sc <- newSieveCache @Int @String 5
      evicted <- insert sc 1 "a"
      evicted `shouldBe` Nothing

    it "update existing key returns Nothing (no eviction)" $ do
      sc <- newSieveCache @Int @String 5
      _ <- insert sc 1 "a"
      evicted <- insert sc 1 "b"
      evicted `shouldBe` Nothing
      val <- lookup sc 1
      val `shouldBe` Just "b"

  describe "eviction" $ do
    it "evicts when at capacity" $ do
      sc <- newSieveCache @Int @String 2
      _ <- insert sc 1 "a"
      _ <- insert sc 2 "b"
      evicted <- insert sc 3 "c"
      case evicted of
        Nothing -> expectationFailure "expected an eviction"
        Just (k, _) -> k `shouldSatisfy` (`elem` [1, 2])
      sz <- size sc
      sz `shouldBe` 2

    it "size never exceeds capacity (deterministic)" $ do
      sc <- newSieveCache @Int @String 3
      mapM_ (\i -> insert sc i (show i)) [1..10 :: Int]
      sz <- size sc
      sz `shouldBe` 3

    it "visited entries survive eviction (second chance)" $ do
      sc <- newSieveCache @Int @String 2
      _ <- insert sc 1 "a"
      _ <- insert sc 2 "b"
      _ <- lookup sc 1
      evicted <- insert sc 3 "c"
      evicted `shouldBe` Just (2, "b")
      val <- lookup sc 1
      val `shouldBe` Just "a"

    it "evicts after all visited bits are reset" $ do
      sc <- newSieveCache @Int @String 2
      _ <- insert sc 1 "a"
      _ <- insert sc 2 "b"
      _ <- lookup sc 1
      _ <- lookup sc 2
      evicted <- insert sc 3 "c"
      case evicted of
        Nothing -> expectationFailure "expected an eviction"
        Just (k, _) -> k `shouldSatisfy` (`elem` [1, 2])
      sz <- size sc
      sz `shouldBe` 2

    it "capacity-1 cache evicts on every insert" $ do
      sc <- newSieveCache @Int @String 1
      _ <- insert sc 1 "a"
      evicted <- insert sc 2 "b"
      evicted `shouldBe` Just (1, "a")
      val <- lookup sc 1
      val `shouldBe` Nothing
      val2 <- lookup sc 2
      val2 `shouldBe` Just "b"

  describe "structural invariants (Hedgehog)" $ do
    it "size never exceeds capacity under arbitrary inserts" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 50))
      ops <- forAll (Gen.list (Range.linear 0 200) (Gen.int (Range.linear 0 100)))
      sz <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        mapM_ (\k -> insert sc k k) ops
        size sc
      assert (sz <= cap)

    it "size is non-negative under arbitrary mixed ops" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 30))
      ops <- forAll (Gen.list (Range.linear 0 150) (genOp (Range.linear 0 50)))
      sz <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        forM_ ops (runOp sc)
        size sc
      assert (sz >= 0)
      assert (sz <= cap)

    it "internal lookup count matches reported size" $ hedgehog $ do
      -- After arbitrary ops, the number of keys (drawn from a known
      -- universe) that lookup says are present must equal the reported size.
      cap <- forAll (Gen.int (Range.linear 1 20))
      let universe = [0 .. 30 :: Int]
      ops <- forAll (Gen.list (Range.linear 0 100) (genOp (Range.linear 0 30)))
      (sz, hits) <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        forM_ ops (runOp sc)
        s <- size sc
        h <- length . filter (== True) <$>
          mapM (\k -> (\m -> case m of Just _ -> True; Nothing -> False) <$> lookup sc k) universe
        pure (s, h)
      sz === hits

    it "size matches number of distinct keys when capacity is unbounded" $ hedgehog $ do
      keys <- forAll (Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 50)))
      sz <- evalIO $ do
        sc <- newSieveCache @Int @Int 1000
        mapM_ (\k -> insert sc k k) keys
        size sc
      sz === length (nub keys)

    it "all lookups after full population return Just" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 20))
      let keys = [0 .. cap - 1]
      results <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        mapM_ (\k -> insert sc k (k * 10)) keys
        mapM (lookup sc) keys
      results === map (\k -> Just (k * 10)) keys

  describe "membership semantics (Hedgehog)" $ do
    it "insert immediately makes the key findable" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 50))
      k <- forAll (Gen.int (Range.linear 0 100))
      v <- forAll (Gen.int (Range.linear 0 1000))
      val <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        _ <- insert sc k v
        lookup sc k
      val === Just v

    it "delete makes a present key absent" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 50))
      k <- forAll (Gen.int (Range.linear 0 100))
      val <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        _ <- insert sc k 42
        delete sc k
        lookup sc k
      val === Nothing

    it "double delete is idempotent" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 30))
      k <- forAll (Gen.int (Range.linear 0 100))
      (sz1, sz2, val) <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        _ <- insert sc k 1
        delete sc k
        s1 <- size sc
        delete sc k
        s2 <- size sc
        v <- lookup sc k
        pure (s1, s2, v)
      sz1 === sz2
      val === Nothing

    it "delete-of-absent is a no-op" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 30))
      keys <- forAll (Gen.list (Range.linear 0 10) (Gen.int (Range.linear 0 50)))
      ghost <- forAll (Gen.int (Range.linear 100 200))  -- disjoint from keys
      (szBefore, szAfter) <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        mapM_ (\k -> insert sc k k) keys
        s1 <- size sc
        delete sc ghost
        s2 <- size sc
        pure (s1, s2)
      szBefore === szAfter

    it "update preserves size" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 2 20))
      k <- forAll (Gen.int (Range.linear 0 100))
      (sz1, sz2) <- evalIO $ do
        sc <- newSieveCache @Int @String cap
        _ <- insert sc k "old"
        s1 <- size sc
        _ <- insert sc k "new"
        s2 <- size sc
        pure (s1, s2)
      sz1 === sz2

    it "update overwrites the stored value" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 20))
      k <- forAll (Gen.int (Range.linear 0 100))
      v1 <- forAll (Gen.int (Range.linear 0 1000))
      v2 <- forAll (Gen.int (Range.linear 1001 2000))
      val <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        _ <- insert sc k v1
        _ <- insert sc k v2
        lookup sc k
      val === Just v2

  describe "eviction signal accuracy (Hedgehog)" $ do
    it "no eviction when inserting under capacity" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 2 30))
      n <- forAll (Gen.int (Range.linear 1 (cap - 1)))
      evictions <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        mapM (\i -> insert sc i i) [0 .. n - 1]
      evictions === replicate n Nothing

    it "no eviction when updating an existing key (regardless of fullness)" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 20))
      let keys = [0 .. cap - 1]
      ev <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        mapM_ (\k -> insert sc k k) keys
        -- Cache is now full. Updating any existing key must not evict.
        mapM (\k -> insert sc k (k + 100)) keys
      ev === replicate cap Nothing

    it "evicted key was previously present" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 10))
      ops <- forAll (Gen.list (Range.linear 1 50) (Gen.int (Range.linear 0 30)))
      evalIO $ do
        sc <- newSieveCache @Int @Int cap
        let go inserted k = do
              ev <- insert sc k k
              case ev of
                Nothing -> pure (k : inserted)
                Just (ek, _) -> do
                  -- Evicted key must have been previously inserted...
                  when (ek `notElem` inserted) $
                    fail $ "evicted key " <> show ek <> " was never inserted"
                  -- ...and must not be findable after eviction.
                  v <- lookup sc ek
                  case v of
                    Just _ -> fail $ "evicted key " <> show ek <> " still present"
                    Nothing -> pure ()
                  pure (k : filter (/= ek) inserted)
        _ <- foldM go [] ops
        pure ()

    it "size + evictions <= total inserts (each insert grows size or evicts, never both)" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 20))
      keys <- forAll (Gen.list (Range.linear 1 80) (Gen.int (Range.linear 0 50)))
      (evictionCount, sz) <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        evs <- mapM (\k -> insert sc k k) keys
        let !ec = length [() | Just _ <- evs]
        s <- size sc
        pure (ec, s)
      assert (sz + evictionCount <= length keys)
      assert (sz <= cap)

    it "with all-distinct keys: size + evictions = total inserts (no updates)" $ hedgehog $ do
      -- Distinct keys means no insert is ever an update, so every insert
      -- either grows the cache by one or evicts one entry.
      cap <- forAll (Gen.int (Range.linear 1 20))
      n <- forAll (Gen.int (Range.linear 1 80))
      let keys = [0 .. n - 1]  -- all distinct
      (evictionCount, sz) <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        evs <- mapM (\k -> insert sc k k) keys
        let !ec = length [() | Just _ <- evs]
        s <- size sc
        pure (ec, s)
      sz + evictionCount === n

  describe "model agreement (Hedgehog state machine)" $ do
    -- After every operation in a random sequence, the SIEVE cache and
    -- the reference model agree on (a) reported size and (b) the
    -- presence and value of every key in the universe.
    it "SIEVE and Map model agree after every operation" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 15))
      let universe = [0 .. 25 :: Int]
      ops <- forAll (Gen.list (Range.linear 0 100) (genOp (Range.linear 0 25)))
      result <- evalIO $ runScenario cap universe ops
      result === Right ()

  describe "second-chance (visited bit) semantics (Hedgehog)" $ do
    it "looking up every key keeps capacity-many entries after one extra insert" $ hedgehog $ do
      -- Fill the cache, look up every entry (sets visited on all),
      -- then insert one new key. Even with all bits set, SIEVE must
      -- still evict exactly one entry (after a sweep that resets bits).
      cap <- forAll (Gen.int (Range.linear 2 30))
      let keys = [0 .. cap - 1]
          newKey = cap
      (sz, ev) <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        mapM_ (\k -> insert sc k k) keys
        mapM_ (lookup sc) keys
        e <- insert sc newKey 999
        s <- size sc
        pure (s, e)
      sz === cap
      case ev of
        Just _ -> success
        Nothing -> failure  -- must have evicted

    it "an unvisited key is preferred for eviction over a visited one" $ hedgehog $ do
      -- Fill cache. Visit only one key. Insert one new key.
      -- The visited key must survive.
      cap <- forAll (Gen.int (Range.linear 2 20))
      let keys = [0 .. cap - 1]
          visitedKey = 0
          newKey = cap
      survived <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        mapM_ (\k -> insert sc k k) keys
        _ <- lookup sc visitedKey
        _ <- insert sc newKey 999
        lookup sc visitedKey
      survived === Just visitedKey

  describe "clear / capacity / foldEntries (Hedgehog)" $ do
    it "capacity reports the value passed to newSieveCache" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 1000))
      reported <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        pure (capacity sc)
      reported === cap

    it "clear empties the cache" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 30))
      ops <- forAll (Gen.list (Range.linear 0 100) (genOp (Range.linear 0 50)))
      let universe = [0 .. 50 :: Int]
      (sz, hits) <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        forM_ ops (runOp sc)
        clear sc
        s <- size sc
        h <- length . filter id <$>
          mapM (\k -> (\m -> case m of Just _ -> True; Nothing -> False) <$> lookup sc k) universe
        pure (s, h)
      sz === 0
      hits === 0

    it "after clear, the cache accepts cap fresh inserts without eviction" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 30))
      preops <- forAll (Gen.list (Range.linear 0 80) (genOp (Range.linear 0 50)))
      evictions <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        forM_ preops (runOp sc)
        clear sc
        mapM (\k -> insert sc k k) [0 .. cap - 1]
      evictions === replicate cap Nothing

    it "foldEntries enumerates exactly `size` keys, all findable by lookup" $ hedgehog $ do
      cap <- forAll (Gen.int (Range.linear 1 30))
      ops <- forAll (Gen.list (Range.linear 0 200) (genOp (Range.linear 0 60)))
      (sz, enumerated, allFound) <- evalIO $ do
        sc <- newSieveCache @Int @Int cap
        forM_ ops (runOp sc)
        s <- size sc
        ks <- foldEntries sc [] (\acc k _ -> pure (k : acc))
        -- Every enumerated key should be findable. lookup mutates the
        -- visited bit but not membership, so this is safe.
        found <- mapM (\k -> lookup sc k) ks
        pure (s, length ks, all (\m -> case m of Just _ -> True; Nothing -> False) found)
      enumerated === sz
      assert allFound

  describe "fuzz" $ do
    -- Long-running fuzzers. These crank up case counts and op-sequence
    -- lengths to surface rare interactions between the visited bit, the
    -- hand pointer, eviction, and updates.

    it "fuzz: model agreement at every step (wide universe)" $
      hedgehog $ do
        cap <- forAll (Gen.int (Range.linear 1 25))
        let universe = [0 .. 40 :: Int]
        ops <- forAll (Gen.list (Range.linear 0 200) (genOp (Range.linear 0 40)))
        result <- evalIO $ runScenario cap universe ops
        result === Right ()

    it "fuzz: model agreement, narrow universe (heavy collisions)" $
      hedgehog $ do
        cap <- forAll (Gen.int (Range.linear 1 8))
        let universe = [0 .. 15 :: Int]
        ops <- forAll (Gen.list (Range.linear 0 200) (genOp (Range.linear 0 15)))
        result <- evalIO $ runScenario cap universe ops
        result === Right ()

    it "fuzz: foldEntries always matches size after long random runs" $
      hedgehog $ do
        cap <- forAll (Gen.int (Range.linear 1 30))
        ops <- forAll (Gen.list (Range.linear 0 250) (genOp (Range.linear 0 60)))
        (sz, enumerated) <- evalIO $ do
          sc <- newSieveCache @Int @Int cap
          forM_ ops (runOp sc)
          s <- size sc
          n <- foldEntries sc 0 (\acc _ _ -> pure (acc + 1))
          pure (s, n)
        enumerated === sz

    it "fuzz: never reports an eviction for a key that wasn't recently present" $
      hedgehog $ do
        cap <- forAll (Gen.int (Range.linear 1 12))
        keys <- forAll (Gen.list (Range.linear 1 300) (Gen.int (Range.linear 0 30)))
        result <- evalIO $ do
          sc <- newSieveCache @Int @Int cap
          let go _ [] = pure (Right () :: Either String ())
              go inserted (k : ks) = do
                ev <- insert sc k k
                case ev of
                  Nothing -> go (k : filter (/= k) inserted) ks
                  Just (ek, ev') -> do
                    if ek `notElem` inserted
                      then pure (Left $ "phantom eviction: " <> show ek
                                  <> " (value " <> show ev' <> ") never inserted")
                      else go (k : filter (/= ek) (filter (/= k) inserted)) ks
          go [] keys
        result === Right ()

    it "fuzz: clear-then-refill cycles preserve invariants" $
      hedgehog $ do
        cap <- forAll (Gen.int (Range.linear 1 20))
        cycles <- forAll (Gen.int (Range.linear 1 20))
        opsPerCycle <- forAll (Gen.int (Range.linear 1 200))
        sizes <- evalIO $ do
          sc <- newSieveCache @Int @Int cap
          let runCycle = do
                forM_ [0 .. opsPerCycle] $ \i -> do
                  let !k = i `mod` (cap * 2)
                  _ <- insert sc k i
                  pure ()
                clear sc
                size sc
          mapM (const runCycle) [1 .. cycles]
        -- Every cycle ends with size 0 because of the trailing 'clear'.
        sizes === replicate cycles 0

    it "fuzz: deleting every key currently in the cache empties it" $
      hedgehog $ do
        cap <- forAll (Gen.int (Range.linear 1 20))
        ops <- forAll (Gen.list (Range.linear 0 300) (genOp (Range.linear 0 40)))
        finalSize <- evalIO $ do
          sc <- newSieveCache @Int @Int cap
          forM_ ops (runOp sc)
          ks <- foldEntries sc [] (\acc k _ -> pure (k : acc))
          forM_ ks (delete sc)
          size sc
        finalSize === 0

    it "fuzz: lookup never resurrects a previously evicted key" $
      hedgehog $ do
        -- Run a sequence of inserts; after each, sweep the universe
        -- looking up every key. Anything we observe missing must stay
        -- missing until it's explicitly re-inserted.
        cap <- forAll (Gen.int (Range.linear 1 8))
        keys <- forAll (Gen.list (Range.linear 1 200) (Gen.int (Range.linear 0 20)))
        let universe = [0 .. 20 :: Int]
        result <- evalIO $ do
          sc <- newSieveCache @Int @Int cap
          let step missing k = do
                _ <- insert sc k k
                -- Sweep universe and rebuild the missing set.
                missing' <- foldM (\acc u -> do
                    m <- lookup sc u
                    case m of
                      Nothing -> pure (u : acc)
                      Just _ -> pure acc) [] universe
                -- Anything in `missing` that we didn't just re-insert
                -- (k) but which is now present must NOT happen.
                let resurrected =
                      [u | u <- missing, u /= k, u `notElem` missing']
                if null resurrected
                  then pure (Right missing')
                  else pure (Left $ "resurrected keys: " <> show resurrected)
              go _ [] = pure (Right () :: Either String ())
              go missing (k : ks) = do
                r <- step missing k
                case r of
                  Left e -> pure (Left e)
                  Right missing' -> go missing' ks
          go [] keys
        result === Right ()

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

runOp :: (Ord k) => SieveCache k v -> Op k v -> IO ()
runOp sc op = case op of
  OpInsert k v -> () <$ insert sc k v
  OpLookup k -> () <$ lookup sc k
  OpDelete k -> delete sc k

------------------------------------------------------------------------
-- Scenario runner: agreement check between SIEVE and the model.
------------------------------------------------------------------------

-- | Run a sequence of operations on both a real SIEVE cache and the
-- reference model, returning Right () if they agree at every step or
-- Left explaining the divergence.
runScenario :: Int -> [Int] -> [Op Int Int] -> IO (Either String ())
runScenario cap universe ops = do
  sc <- newSieveCache @Int @Int cap
  modelRef <- newIORef (newModel cap)
  go sc modelRef ops
  where
    go _ _ [] = pure (Right ())
    go sc modelRef (op : rest) = do
      m0 <- readIORef modelRef
      case op of
        OpLookup k -> do
          actual <- lookup sc k
          let expected = modelLookup k m0
          if actual /= expected
            then pure (Left $ "lookup " <> show k <> ": SIEVE=" <> show actual
                        <> " model=" <> show expected)
            else checkInvariants sc m0 universe op >>= \case
              Left e -> pure (Left e)
              Right () -> go sc modelRef rest
        OpInsert k v -> do
          ev <- insert sc k v
          let m1 = modelInsert k v ev m0
          writeIORef modelRef m1
          checkInvariants sc m1 universe op >>= \case
            Left e -> pure (Left e)
            Right () -> go sc modelRef rest
        OpDelete k -> do
          delete sc k
          let m1 = modelDelete k m0
          writeIORef modelRef m1
          checkInvariants sc m1 universe op >>= \case
            Left e -> pure (Left e)
            Right () -> go sc modelRef rest

    checkInvariants sc m universe' op = do
      sz <- size sc
      if sz /= modelSize m
        then pure (Left $ "after " <> show op
                  <> ": size=" <> show sz <> " modelSize=" <> show (modelSize m))
        else do
          mismatches <- foldM (\acc k -> do
              actual <- lookup sc k
              -- N.B. lookup mutates SIEVE state (sets visited bit) but
              -- doesn't change membership, so iterating universe is safe.
              let expected = modelLookup k m
              pure $ if actual == expected
                then acc
                else (k, actual, expected) : acc
            ) [] universe'
          case mismatches of
            [] -> pure (Right ())
            ((k, a, e) : _) ->
              pure (Left $ "after " <> show op <> ": key " <> show k
                    <> " SIEVE=" <> show a <> " model=" <> show e)

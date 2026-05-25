{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Microbenchmarks for the SIEVE-based prepared-statement cache
-- ('PgWire.Cache.Sieve'), used per-connection by 'Valiant.Execute' and
-- 'PgWire.Connection'.
module Main where

import Prelude hiding (lookup)

import Control.DeepSeq (NFData (..))
import Control.Monad (forM_, void)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import PgWire.Cache.Sieve (SieveCache, insert, lookup, newSieveCache)
import System.Random (mkStdGen, randomR)

-- 'SieveCache' is IORef-backed; for criterion's 'env' we just need a
-- handle that survives 'evaluate'. Forcing the IORefs would mutate
-- nothing useful, so we treat them as opaque.
instance NFData (SieveCache k v) where
  rnf _ = ()

------------------------------------------------------------------------
-- Workload generators
------------------------------------------------------------------------

mkKey :: Int -> ByteString
mkKey i = BS.pack (replicate (10 - length s) '0' ++ s)
  where s = show i

sequentialKeys :: Int -> [ByteString]
sequentialKeys n = map mkKey [0 .. n - 1]

uniformKeys :: Int -> Int -> [ByteString]
uniformKeys maxKey n = take n (go (mkStdGen 0xCAFE))
  where
    go g = let (k, g') = randomR (0, maxKey - 1) g in mkKey k : go g'

zipfKeys :: Int -> Int -> [ByteString]
zipfKeys maxKey n =
  let !table = zipfCdf maxKey 1.0
   in take n (go (mkStdGen 0xBEEF) table)
  where
    go g table =
      let (u, g') = randomR (0.0 :: Double, 1.0) g
          k = invertCdf table u
       in mkKey k : go g' table

zipfCdf :: Int -> Double -> [(Int, Double)]
zipfCdf maxKey s =
  let weights = [(i, 1 / fromIntegral i ** s) | i <- [1 .. maxKey]]
      total = sum (map snd weights)
   in scanl1 acc [(i, w / total) | (i, w) <- weights]
  where
    acc (_, c) (i, w) = (i, c + w)

invertCdf :: [(Int, Double)] -> Double -> Int
invertCdf [] _ = 0
invertCdf [(i, _)] _ = i
invertCdf ((i, c) : rest) u
  | u <= c = i
  | otherwise = invertCdf rest u

------------------------------------------------------------------------
-- Cache drivers
------------------------------------------------------------------------

runSieveInserts :: Int -> [ByteString] -> IO ()
runSieveInserts cap ks = do
  sc <- newSieveCache @ByteString @ByteString cap
  forM_ ks $ \k -> void (insert sc k k)

runSieveLookups :: SieveCache ByteString ByteString -> [ByteString] -> IO ()
runSieveLookups sc ks = forM_ ks (\k -> void (lookup sc k))

runSieveMixed :: Int -> [ByteString] -> IO ()
runSieveMixed cap ks = do
  sc <- newSieveCache @ByteString @ByteString cap
  forM_ ks $ \k -> do
    m <- lookup sc k
    case m of
      Just _ -> pure ()
      Nothing -> void (insert sc k k)

warmSieve :: Int -> [ByteString] -> IO (SieveCache ByteString ByteString)
warmSieve cap ks = do
  sc <- newSieveCache @ByteString @ByteString cap
  forM_ ks (\k -> void (insert sc k k))
  pure sc

------------------------------------------------------------------------
-- Workloads
------------------------------------------------------------------------

defaultCap :: Int
defaultCap = 100

opsCount :: Int
opsCount = 2000

main :: IO ()
main = do
  let !keysFill = sequentialKeys (defaultCap `div` 2)
      !keysFull = sequentialKeys defaultCap
      !keysSeq2x = sequentialKeys (defaultCap * 2)
      !keysHit = take opsCount (cycle keysFull)
      !keysSweep = take opsCount (cycle keysSeq2x)
      !keysMixed = take opsCount (cycle keysFull)
      !keysZipf = zipfKeys (defaultCap * 4) opsCount
      !keysUniform4x = uniformKeys (defaultCap * 4) opsCount

  defaultMain
    [ bench "fill (under capacity, no evictions)" $
        whnfIO (runSieveInserts defaultCap keysFill)
    , bench "fill (exactly to capacity)" $
        whnfIO (runSieveInserts defaultCap keysFull)
    , env (warmSieve defaultCap keysFull) $ \sc ->
        bench "hit-only (warm cache, all lookups hit)" $
          whnfIO (runSieveLookups sc keysHit)
    , bench "mixed lookup/insert (working set = cap, all hit after warmup)" $
        whnfIO (runSieveMixed defaultCap keysMixed)
    , bench "eviction storm (sequential, working set 2x cap)" $
        whnfIO (runSieveMixed defaultCap keysSweep)
    , bench "uniform random access (working set 4x cap)" $
        whnfIO (runSieveMixed defaultCap keysUniform4x)
    , bench "Zipf-skewed access (working set 4x cap, s=1.0)" $
        whnfIO (runSieveMixed defaultCap keysZipf)
    ]

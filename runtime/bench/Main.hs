module Main where

import Criterion.Main
import qualified BenchCodec
import qualified BenchConcurrent
import qualified BenchPool
import qualified BenchQuery
import System.Environment (lookupEnv)

main :: IO ()
main = do
  mUrl <- lookupEnv "DATABASE_URL"
  let dbBenches = case mUrl of
        Nothing -> []
        Just _ -> BenchQuery.benchmarks <> BenchConcurrent.benchmarks <> BenchPool.benchmarks
  defaultMain $
    [ bgroup "codec" BenchCodec.benchmarks
    ] <> dbBenches

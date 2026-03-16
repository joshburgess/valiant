module Main where

import Criterion.Main
import qualified BenchCodec
import qualified BenchQuery
import System.Environment (lookupEnv)

main :: IO ()
main = do
  mUrl <- lookupEnv "DATABASE_URL"
  let dbBenches = case mUrl of
        Nothing -> []
        Just _ -> BenchQuery.benchmarks
  defaultMain $
    [ bgroup "codec" BenchCodec.benchmarks
    ] <> dbBenches

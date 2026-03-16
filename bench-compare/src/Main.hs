module Main where

import Criterion.Main
import Data.ByteString.Char8 qualified as BS8
import System.Environment (lookupEnv)

import qualified BenchHsqlx
import qualified BenchHasql
import qualified BenchPgSimple

main :: IO ()
main = do
  mUrl <- lookupEnv "DATABASE_URL"
  case mUrl of
    Nothing -> do
      putStrLn "DATABASE_URL is not set."
      putStrLn "  Run: eval $(scripts/pg-setup.sh)"
      putStrLn "  Then: cabal run bench-compare"
    Just url -> do
      let bs = BS8.pack url
      putStrLn $ "Connecting to: " <> take 40 url <> "..."
      putStrLn "Setting up schema and seed data..."
      BenchHsqlx.setup bs
      putStrLn "Running benchmarks..."
      defaultMain
        [ bgroup "SELECT 1"
            [ bench "hsqlx"             $ whnfIO (BenchHsqlx.selectOne bs)
            , bench "hasql"             $ whnfIO (BenchHasql.selectOne url)
            , bench "postgresql-simple" $ whnfIO (BenchPgSimple.selectOne bs)
            ]
        , bgroup "fetchOne by PK"
            [ bench "hsqlx"             $ whnfIO (BenchHsqlx.fetchOneByPK bs)
            , bench "hasql"             $ whnfIO (BenchHasql.fetchOneByPK url)
            , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchOneByPK bs)
            ]
        , bgroup "fetchAll 1000 rows"
            [ bench "hsqlx"             $ whnfIO (BenchHsqlx.fetchAll1000 bs)
            , bench "hasql"             $ whnfIO (BenchHasql.fetchAll1000 url)
            , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchAll1000 bs)
            ]
        , bgroup "INSERT (single row)"
            [ bench "hsqlx"             $ whnfIO (BenchHsqlx.insertOne bs)
            , bench "hasql"             $ whnfIO (BenchHasql.insertOne url)
            , bench "postgresql-simple" $ whnfIO (BenchPgSimple.insertOne bs)
            ]
        ]

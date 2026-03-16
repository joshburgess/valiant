module Main where

import Criterion
import Criterion.Main (defaultMainWith, defaultConfig)
import Criterion.Types (Config(..))
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
      putStrLn "Setting up schema and seeding 10000 rows..."
      BenchHsqlx.setup bs 10000

      putStrLn "Running benchmarks..."
      let cfg = defaultConfig { timeLimit = 3 }
      defaultMainWith cfg
        -- ── Single-row operations ─────────────────────────────────
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

        -- ── Fetch N rows ──────────────────────────────────────────
        , bgroup "fetch 1000 rows"
            [ bench "hsqlx"             $ whnfIO (BenchHsqlx.fetchN bs 1000)
            , bench "hasql"             $ whnfIO (BenchHasql.fetchN url 1000)
            , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchN bs 1000)
            ]
        , bgroup "fetch 5000 rows"
            [ bench "hsqlx"             $ whnfIO (BenchHsqlx.fetchN bs 5000)
            , bench "hasql"             $ whnfIO (BenchHasql.fetchN url 5000)
            , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchN bs 5000)
            ]
        , bgroup "fetch 10000 rows"
            [ bench "hsqlx"             $ whnfIO (BenchHsqlx.fetchN bs 10000)
            , bench "hasql"             $ whnfIO (BenchHasql.fetchN url 10000)
            , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchN bs 10000)
            ]

        -- ── Inserts ───────────────────────────────────────────────
        , bgroup "insert 100 rows"
            [ bench "hsqlx (pipelined)" $ whnfIO (BenchHsqlx.insertNPipelined bs 100)
            , bench "hsqlx (sequential)" $ whnfIO (BenchHsqlx.insertN bs 100)
            , bench "hasql"             $ whnfIO (BenchHasql.insertN url 100)
            , bench "postgresql-simple" $ whnfIO (BenchPgSimple.insertN bs 100)
            ]
        , bgroup "insert 1000 rows"
            [ bench "hsqlx (pipelined)" $ whnfIO (BenchHsqlx.insertNPipelined bs 1000)
            , bench "hsqlx (sequential)" $ whnfIO (BenchHsqlx.insertN bs 1000)
            , bench "hasql"             $ whnfIO (BenchHasql.insertN url 1000)
            , bench "postgresql-simple" $ whnfIO (BenchPgSimple.insertN bs 1000)
            ]
        , bgroup "insert 5000 rows"
            [ bench "hsqlx (pipelined)" $ whnfIO (BenchHsqlx.insertNPipelined bs 5000)
            , bench "hsqlx (sequential)" $ whnfIO (BenchHsqlx.insertN bs 5000)
            ]

        -- ── Updates (single UPDATE affecting N rows) ─────────────
        , bgroup "update 100 rows"
            [ bench "hsqlx"             $ whnfIO (BenchHsqlx.updateN bs 100)
            , bench "hasql"             $ whnfIO (BenchHasql.updateN url 100)
            , bench "postgresql-simple" $ whnfIO (BenchPgSimple.updateN bs 100)
            ]
        , bgroup "update 1000 rows"
            [ bench "hsqlx"             $ whnfIO (BenchHsqlx.updateN bs 1000)
            , bench "hasql"             $ whnfIO (BenchHasql.updateN url 1000)
            , bench "postgresql-simple" $ whnfIO (BenchPgSimple.updateN bs 1000)
            ]
        , bgroup "update 5000 rows"
            [ bench "hsqlx"             $ whnfIO (BenchHsqlx.updateN bs 5000)
            ]
        ]

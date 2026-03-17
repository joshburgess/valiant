module Main where

import Criterion
import Criterion.Main (defaultMainWith, defaultConfig)
import Criterion.Types (Config(..))
import Data.ByteString.Char8 qualified as BS8
import System.Environment (lookupEnv)

import qualified BenchHsqlx
import qualified BenchHasql
import qualified BenchPgSimple
import qualified BenchPersistent

main :: IO ()
main = do
  mUrl <- lookupEnv "DATABASE_URL"
  case mUrl of
    Nothing -> do
      putStrLn "DATABASE_URL is not set."
      putStrLn "  Run: eval $(scripts/pg-setup.sh)"
      putStrLn "  Then: cabal run bench-compare"
      putStrLn ""
      putStrLn "Set BENCH_MODE env var to select benchmarks:"
      putStrLn "  all | reads | inserts | updates | quick | hsqlx-only"
    Just url -> do
      mMode <- lookupEnv "BENCH_MODE"
      let bs = BS8.pack url
          mode = maybe "all" id mMode

      putStrLn $ "Connecting to: " <> take 40 url <> "..."
      putStrLn $ "Mode: " <> mode
      putStrLn "Setting up schema and seeding 10000 rows..."
      BenchHsqlx.setup bs 10000

      let cfg = defaultConfig { timeLimit = cfgTimeLimit mode }
      putStrLn "Running benchmarks..."
      defaultMainWith cfg (selectBenchmarks mode bs url)

-- | Time limit per benchmark. Lower for slow benchmarks.
cfgTimeLimit :: String -> Double
cfgTimeLimit "quick" = 1
cfgTimeLimit _ = 3

-- | Select which benchmarks to run based on mode.
selectBenchmarks :: String -> BS8.ByteString -> String -> [Benchmark]
selectBenchmarks mode bs url = case mode of
  "reads"     -> readBenches bs url
  "inserts"   -> insertBenches bs url
  "updates"   -> updateBenches bs url
  "pipeline"  -> pipelineBenches bs
  "quick"     -> quickBenches bs url
  "hsqlx-only" -> hsqlxOnlyBenches bs
  _           -> readBenches bs url <> insertBenches bs url <> updateBenches bs url <> pipelineBenches bs

-- ── Read benchmarks ──────────────────────────────────────────────

readBenches :: BS8.ByteString -> String -> [Benchmark]
readBenches bs url =
  [ bgroup "SELECT 1"
      [ bench "hsqlx"             $ whnfIO (BenchHsqlx.selectOne bs)
      , bench "hasql"             $ whnfIO (BenchHasql.selectOne url)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.selectOne bs)
      , bench "persistent"        $ whnfIO (BenchPersistent.selectOne bs)
      ]
  , bgroup "fetchOne by PK"
      [ bench "hsqlx"             $ whnfIO (BenchHsqlx.fetchOneByPK bs)
      , bench "hasql"             $ whnfIO (BenchHasql.fetchOneByPK url)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchOneByPK bs)
      , bench "persistent"        $ whnfIO (BenchPersistent.fetchOneByPK bs)
      ]
  , bgroup "fetch 1000 rows"
      [ bench "hsqlx"             $ whnfIO (BenchHsqlx.fetchN bs 1000)
      , bench "hasql"             $ whnfIO (BenchHasql.fetchN url 1000)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchN bs 1000)
      , bench "persistent"        $ whnfIO (BenchPersistent.fetchN bs 1000)
      ]
  , bgroup "fetch 5000 rows"
      [ bench "hsqlx"             $ whnfIO (BenchHsqlx.fetchN bs 5000)
      , bench "hasql"             $ whnfIO (BenchHasql.fetchN url 5000)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchN bs 5000)
      , bench "persistent"        $ whnfIO (BenchPersistent.fetchN bs 5000)
      ]
  , bgroup "fetch 10000 rows"
      [ bench "hsqlx"             $ whnfIO (BenchHsqlx.fetchN bs 10000)
      , bench "hasql"             $ whnfIO (BenchHasql.fetchN url 10000)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchN bs 10000)
      , bench "persistent"        $ whnfIO (BenchPersistent.fetchN bs 10000)
      ]
  ]

-- ── Insert benchmarks ────────────────────────────────────────────

insertBenches :: BS8.ByteString -> String -> [Benchmark]
insertBenches bs url =
  [ bgroup "insert 100 rows"
      [ bench "hsqlx (pipelined)"  $ whnfIO (BenchHsqlx.insertNPipelined bs 100)
      , bench "hsqlx (sequential)" $ whnfIO (BenchHsqlx.insertN bs 100)
      , bench "hasql"              $ whnfIO (BenchHasql.insertN url 100)
      , bench "postgresql-simple"  $ whnfIO (BenchPgSimple.insertN bs 100)
      , bench "persistent"         $ whnfIO (BenchPersistent.insertN bs 100)
      ]
  , bgroup "insert 1000 rows"
      [ bench "hsqlx (pipelined)"  $ whnfIO (BenchHsqlx.insertNPipelined bs 1000)
      , bench "hsqlx (sequential)" $ whnfIO (BenchHsqlx.insertN bs 1000)
      , bench "hasql"              $ whnfIO (BenchHasql.insertN url 1000)
      , bench "postgresql-simple"  $ whnfIO (BenchPgSimple.insertN bs 1000)
      , bench "persistent"         $ whnfIO (BenchPersistent.insertN bs 1000)
      ]
  , bgroup "insert 5000 rows"
      [ bench "hsqlx (pipelined)"  $ whnfIO (BenchHsqlx.insertNPipelined bs 5000)
      , bench "hsqlx (sequential)" $ whnfIO (BenchHsqlx.insertN bs 5000)
      , bench "hasql"              $ whnfIO (BenchHasql.insertN url 5000)
      , bench "postgresql-simple"  $ whnfIO (BenchPgSimple.insertN bs 5000)
      , bench "persistent"         $ whnfIO (BenchPersistent.insertN bs 5000)
      ]
  ]

-- ── Update benchmarks ────────────────────────────────────────────

updateBenches :: BS8.ByteString -> String -> [Benchmark]
updateBenches bs url =
  [ bgroup "update 100 rows"
      [ bench "hsqlx"             $ whnfIO (BenchHsqlx.updateN bs 100)
      , bench "hasql"             $ whnfIO (BenchHasql.updateN url 100)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.updateN bs 100)
      , bench "persistent"        $ whnfIO (BenchPersistent.updateN bs 100)
      ]
  , bgroup "update 1000 rows"
      [ bench "hsqlx"             $ whnfIO (BenchHsqlx.updateN bs 1000)
      , bench "hasql"             $ whnfIO (BenchHasql.updateN url 1000)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.updateN bs 1000)
      , bench "persistent"        $ whnfIO (BenchPersistent.updateN bs 1000)
      ]
  , bgroup "update 5000 rows"
      [ bench "hsqlx"             $ whnfIO (BenchHsqlx.updateN bs 5000)
      , bench "hasql"             $ whnfIO (BenchHasql.updateN url 5000)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.updateN bs 5000)
      , bench "persistent"        $ whnfIO (BenchPersistent.updateN bs 5000)
      ]
  ]

-- ── Pipeline benchmarks (hsqlx-only feature) ─────────────────────

pipelineBenches :: BS8.ByteString -> [Benchmark]
pipelineBenches bs =
  [ bgroup "2 queries"
      [ bench "pipelined (1 round-trip)"  $ whnfIO (BenchHsqlx.pipelinedReads2 bs)
      , bench "sequential (2 round-trips)" $ whnfIO (BenchHsqlx.sequentialReads2 bs)
      ]
  , bgroup "3 queries"
      [ bench "pipelined (1 round-trip)"  $ whnfIO (BenchHsqlx.pipelinedReads3 bs)
      , bench "sequential (3 round-trips)" $ whnfIO (BenchHsqlx.sequentialReads3 bs)
      ]
  , bgroup "5 queries"
      [ bench "pipelined (1 round-trip)"  $ whnfIO (BenchHsqlx.pipelinedReads5 bs)
      , bench "sequential (5 round-trips)" $ whnfIO (BenchHsqlx.sequentialReads5 bs)
      ]
  ]

-- ── Quick mode (skip slow 5K sequential inserts) ─────────────────

quickBenches :: BS8.ByteString -> String -> [Benchmark]
quickBenches bs url =
  readBenches bs url
    <> [ bgroup "insert 100 rows"
          [ bench "hsqlx (pipelined)"  $ whnfIO (BenchHsqlx.insertNPipelined bs 100)
          , bench "hsqlx (sequential)" $ whnfIO (BenchHsqlx.insertN bs 100)
          , bench "hasql"              $ whnfIO (BenchHasql.insertN url 100)
          , bench "postgresql-simple"  $ whnfIO (BenchPgSimple.insertN bs 100)
          , bench "persistent"         $ whnfIO (BenchPersistent.insertN bs 100)
          ]
       , bgroup "insert 1000 rows (pipelined)"
          [ bench "hsqlx (pipelined)"  $ whnfIO (BenchHsqlx.insertNPipelined bs 1000)
          ]
       , bgroup "update 100 rows"
          [ bench "hsqlx"             $ whnfIO (BenchHsqlx.updateN bs 100)
          , bench "hasql"             $ whnfIO (BenchHasql.updateN url 100)
          , bench "postgresql-simple" $ whnfIO (BenchPgSimple.updateN bs 100)
          , bench "persistent"        $ whnfIO (BenchPersistent.updateN bs 100)
          ]
       ]

-- ── hsqlx-only mode (no competitors) ─────────────────────────────

hsqlxOnlyBenches :: BS8.ByteString -> [Benchmark]
hsqlxOnlyBenches bs =
  [ bgroup "hsqlx SELECT 1"         [ bench "hsqlx" $ whnfIO (BenchHsqlx.selectOne bs) ]
  , bgroup "hsqlx fetchOne by PK"   [ bench "hsqlx" $ whnfIO (BenchHsqlx.fetchOneByPK bs) ]
  , bgroup "hsqlx fetch 1000 rows"  [ bench "hsqlx" $ whnfIO (BenchHsqlx.fetchN bs 1000) ]
  , bgroup "hsqlx fetch 5000 rows"  [ bench "hsqlx" $ whnfIO (BenchHsqlx.fetchN bs 5000) ]
  , bgroup "hsqlx fetch 10000 rows" [ bench "hsqlx" $ whnfIO (BenchHsqlx.fetchN bs 10000) ]
  , bgroup "hsqlx insert 100 pipelined"  [ bench "hsqlx" $ whnfIO (BenchHsqlx.insertNPipelined bs 100) ]
  , bgroup "hsqlx insert 1000 pipelined" [ bench "hsqlx" $ whnfIO (BenchHsqlx.insertNPipelined bs 1000) ]
  , bgroup "hsqlx insert 5000 pipelined" [ bench "hsqlx" $ whnfIO (BenchHsqlx.insertNPipelined bs 5000) ]
  , bgroup "hsqlx update 100 rows"  [ bench "hsqlx" $ whnfIO (BenchHsqlx.updateN bs 100) ]
  , bgroup "hsqlx update 1000 rows" [ bench "hsqlx" $ whnfIO (BenchHsqlx.updateN bs 1000) ]
  , bgroup "hsqlx update 5000 rows" [ bench "hsqlx" $ whnfIO (BenchHsqlx.updateN bs 5000) ]
  ]

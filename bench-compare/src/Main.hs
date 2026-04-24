module Main where

import Criterion
import Criterion.Main (defaultMainWith, defaultConfig)
import Criterion.Types (Config(..))
import Data.ByteString.Char8 qualified as BS8
import System.Environment (lookupEnv)

import qualified BenchValiant
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
      putStrLn "  all | reads | inserts | updates | quick | valiant-only"
    Just url -> do
      mMode <- lookupEnv "BENCH_MODE"
      let bs = BS8.pack url
          mode = maybe "all" id mMode

      putStrLn $ "Connecting to: " <> take 40 url <> "..."
      putStrLn $ "Mode: " <> mode
      putStrLn "Setting up schema and seeding 10000 rows..."
      BenchValiant.setup bs 10000

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
  "valiant-only" -> valiantOnlyBenches bs
  _           -> readBenches bs url <> insertBenches bs url <> updateBenches bs url <> pipelineBenches bs

-- ── Read benchmarks ──────────────────────────────────────────────

readBenches :: BS8.ByteString -> String -> [Benchmark]
readBenches bs url =
  [ bgroup "SELECT 1"
      [ bench "valiant"             $ whnfIO (BenchValiant.selectOne bs)
      , bench "hasql"             $ whnfIO (BenchHasql.selectOne url)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.selectOne bs)
      , bench "persistent"        $ whnfIO (BenchPersistent.selectOne bs)
      ]
  , bgroup "fetchOne by PK"
      [ bench "valiant"             $ whnfIO (BenchValiant.fetchOneByPK bs)
      , bench "hasql"             $ whnfIO (BenchHasql.fetchOneByPK url)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchOneByPK bs)
      , bench "persistent"        $ whnfIO (BenchPersistent.fetchOneByPK bs)
      ]
  , bgroup "fetch 1000 rows"
      [ bench "valiant"             $ whnfIO (BenchValiant.fetchN bs 1000)
      , bench "hasql"             $ whnfIO (BenchHasql.fetchN url 1000)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchN bs 1000)
      , bench "persistent"        $ whnfIO (BenchPersistent.fetchN bs 1000)
      ]
  , bgroup "fetch 5000 rows"
      [ bench "valiant"             $ whnfIO (BenchValiant.fetchN bs 5000)
      , bench "hasql"             $ whnfIO (BenchHasql.fetchN url 5000)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchN bs 5000)
      , bench "persistent"        $ whnfIO (BenchPersistent.fetchN bs 5000)
      ]
  , bgroup "fetch 10000 rows"
      [ bench "valiant"             $ whnfIO (BenchValiant.fetchN bs 10000)
      , bench "hasql"             $ whnfIO (BenchHasql.fetchN url 10000)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.fetchN bs 10000)
      , bench "persistent"        $ whnfIO (BenchPersistent.fetchN bs 10000)
      ]
  ]

-- ── Insert benchmarks ────────────────────────────────────────────

insertBenches :: BS8.ByteString -> String -> [Benchmark]
insertBenches bs url =
  [ bgroup "insert 100 rows"
      [ bench "valiant (pipelined)"  $ whnfIO (BenchValiant.insertNPipelined bs 100)
      , bench "valiant (sequential)" $ whnfIO (BenchValiant.insertN bs 100)
      , bench "hasql"              $ whnfIO (BenchHasql.insertN url 100)
      , bench "postgresql-simple"  $ whnfIO (BenchPgSimple.insertN bs 100)
      , bench "persistent"         $ whnfIO (BenchPersistent.insertN bs 100)
      ]
  , bgroup "insert 1000 rows"
      [ bench "valiant (pipelined)"  $ whnfIO (BenchValiant.insertNPipelined bs 1000)
      , bench "valiant (sequential)" $ whnfIO (BenchValiant.insertN bs 1000)
      , bench "hasql"              $ whnfIO (BenchHasql.insertN url 1000)
      , bench "postgresql-simple"  $ whnfIO (BenchPgSimple.insertN bs 1000)
      , bench "persistent"         $ whnfIO (BenchPersistent.insertN bs 1000)
      ]
  , bgroup "insert 5000 rows"
      [ bench "valiant (pipelined)"  $ whnfIO (BenchValiant.insertNPipelined bs 5000)
      , bench "valiant (sequential)" $ whnfIO (BenchValiant.insertN bs 5000)
      , bench "hasql"              $ whnfIO (BenchHasql.insertN url 5000)
      , bench "postgresql-simple"  $ whnfIO (BenchPgSimple.insertN bs 5000)
      , bench "persistent"         $ whnfIO (BenchPersistent.insertN bs 5000)
      ]
  ]

-- ── Update benchmarks ────────────────────────────────────────────

updateBenches :: BS8.ByteString -> String -> [Benchmark]
updateBenches bs url =
  [ bgroup "update 100 rows"
      [ bench "valiant"             $ whnfIO (BenchValiant.updateN bs 100)
      , bench "hasql"             $ whnfIO (BenchHasql.updateN url 100)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.updateN bs 100)
      , bench "persistent"        $ whnfIO (BenchPersistent.updateN bs 100)
      ]
  , bgroup "update 1000 rows"
      [ bench "valiant"             $ whnfIO (BenchValiant.updateN bs 1000)
      , bench "hasql"             $ whnfIO (BenchHasql.updateN url 1000)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.updateN bs 1000)
      , bench "persistent"        $ whnfIO (BenchPersistent.updateN bs 1000)
      ]
  , bgroup "update 5000 rows"
      [ bench "valiant"             $ whnfIO (BenchValiant.updateN bs 5000)
      , bench "hasql"             $ whnfIO (BenchHasql.updateN url 5000)
      , bench "postgresql-simple" $ whnfIO (BenchPgSimple.updateN bs 5000)
      , bench "persistent"        $ whnfIO (BenchPersistent.updateN bs 5000)
      ]
  ]

-- ── Pipeline benchmarks (valiant-only feature) ─────────────────────

pipelineBenches :: BS8.ByteString -> [Benchmark]
pipelineBenches bs =
  [ bgroup "2 queries"
      [ bench "pipelined (1 round-trip)"  $ whnfIO (BenchValiant.pipelinedReads2 bs)
      , bench "sequential (2 round-trips)" $ whnfIO (BenchValiant.sequentialReads2 bs)
      ]
  , bgroup "3 queries"
      [ bench "pipelined (1 round-trip)"  $ whnfIO (BenchValiant.pipelinedReads3 bs)
      , bench "sequential (3 round-trips)" $ whnfIO (BenchValiant.sequentialReads3 bs)
      ]
  , bgroup "5 queries"
      [ bench "pipelined (1 round-trip)"  $ whnfIO (BenchValiant.pipelinedReads5 bs)
      , bench "sequential (5 round-trips)" $ whnfIO (BenchValiant.sequentialReads5 bs)
      ]
  ]

-- ── Quick mode (skip slow 5K sequential inserts) ─────────────────

quickBenches :: BS8.ByteString -> String -> [Benchmark]
quickBenches bs url =
  readBenches bs url
    <> [ bgroup "insert 100 rows"
          [ bench "valiant (pipelined)"  $ whnfIO (BenchValiant.insertNPipelined bs 100)
          , bench "valiant (sequential)" $ whnfIO (BenchValiant.insertN bs 100)
          , bench "hasql"              $ whnfIO (BenchHasql.insertN url 100)
          , bench "postgresql-simple"  $ whnfIO (BenchPgSimple.insertN bs 100)
          , bench "persistent"         $ whnfIO (BenchPersistent.insertN bs 100)
          ]
       , bgroup "insert 1000 rows (pipelined)"
          [ bench "valiant (pipelined)"  $ whnfIO (BenchValiant.insertNPipelined bs 1000)
          ]
       , bgroup "update 100 rows"
          [ bench "valiant"             $ whnfIO (BenchValiant.updateN bs 100)
          , bench "hasql"             $ whnfIO (BenchHasql.updateN url 100)
          , bench "postgresql-simple" $ whnfIO (BenchPgSimple.updateN bs 100)
          , bench "persistent"        $ whnfIO (BenchPersistent.updateN bs 100)
          ]
       ]

-- ── valiant-only mode (no competitors) ─────────────────────────────

valiantOnlyBenches :: BS8.ByteString -> [Benchmark]
valiantOnlyBenches bs =
  [ bgroup "valiant SELECT 1"         [ bench "valiant" $ whnfIO (BenchValiant.selectOne bs) ]
  , bgroup "valiant fetchOne by PK"   [ bench "valiant" $ whnfIO (BenchValiant.fetchOneByPK bs) ]
  , bgroup "valiant fetch 1000 rows"  [ bench "valiant" $ whnfIO (BenchValiant.fetchN bs 1000) ]
  , bgroup "valiant fetch 5000 rows"  [ bench "valiant" $ whnfIO (BenchValiant.fetchN bs 5000) ]
  , bgroup "valiant fetch 10000 rows" [ bench "valiant" $ whnfIO (BenchValiant.fetchN bs 10000) ]
  , bgroup "valiant insert 100 pipelined"  [ bench "valiant" $ whnfIO (BenchValiant.insertNPipelined bs 100) ]
  , bgroup "valiant insert 1000 pipelined" [ bench "valiant" $ whnfIO (BenchValiant.insertNPipelined bs 1000) ]
  , bgroup "valiant insert 5000 pipelined" [ bench "valiant" $ whnfIO (BenchValiant.insertNPipelined bs 5000) ]
  , bgroup "valiant update 100 rows"  [ bench "valiant" $ whnfIO (BenchValiant.updateN bs 100) ]
  , bgroup "valiant update 1000 rows" [ bench "valiant" $ whnfIO (BenchValiant.updateN bs 1000) ]
  , bgroup "valiant update 5000 rows" [ bench "valiant" $ whnfIO (BenchValiant.updateN bs 5000) ]
  ]

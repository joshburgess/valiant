# bench-compare

Comparative benchmarks of valiant against the rest of the Haskell PostgreSQL
ecosystem (hasql, postgresql-simple, persistent) and against asyncpg (Python).

This package is intentionally not on Hackage. It exists in-repo so the
numbers in [`../docs/benchmark-results/`](../docs/benchmark-results/)
are reproducible and survive driver upgrades.

## What it benchmarks

| Group | Operation | Drivers compared |
|-------|-----------|------------------|
| Reads | `SELECT 1`, fetch by PK, fetch 1k/5k/10k rows | valiant, hasql, postgresql-simple, persistent |
| Inserts | 100/1000/5000 row inserts | valiant (pipelined + sequential), hasql, postgresql-simple, persistent |
| Updates | 100/1000/5000 row updates | valiant, hasql, postgresql-simple, persistent |
| Pipeline | 2/3/5-query pipelined reads | valiant only (feature unique to valiant) |

## Running locally

```bash
# 1. Start a tuned Postgres (Docker; fsync=off, synchronous_commit=off)
eval $(scripts/pg-setup.sh)

# 2. Run all benchmarks (~10 minutes)
cabal run bench-compare

# 3. Or pick a subset via BENCH_MODE
BENCH_MODE=quick   cabal run bench-compare   # ~2 min, no 5k inserts
BENCH_MODE=reads   cabal run bench-compare
BENCH_MODE=inserts cabal run bench-compare
BENCH_MODE=updates cabal run bench-compare
BENCH_MODE=pipeline      cabal run bench-compare
BENCH_MODE=valiant-only  cabal run bench-compare   # no competitors

# 4. Save CSV for archival
cabal run bench-compare -- --csv comparative.csv
```

## Running asyncpg (Python) head-to-head

```bash
pip install asyncpg uvloop
python bench-compare/asyncpg-bench.py "$DATABASE_URL"
```

Same tuned Postgres, same socket, same query patterns as the Haskell suite.

## Running in CI

The workflow at [`.github/workflows/benchmarks.yml`](../.github/workflows/benchmarks.yml)
runs the full suite on `ubuntu-24.04` against a native Postgres 16 over
Unix socket. Trigger it manually from the GitHub Actions tab or via:

```bash
gh workflow run benchmarks.yml -f mode=quick
gh workflow run benchmarks.yml -f mode=comparative
gh workflow run benchmarks.yml -f mode=all
```

Results land in run artifacts (`bench-comparative`, `bench-asyncpg-valiant`,
`bench-asyncpg-baseline`, etc.) and a markdown summary in the run page.

## Latest published numbers

See [`../docs/benchmark-results/`](../docs/benchmark-results/) for the
canonical, archived results:

- [`full-results.md`](../docs/benchmark-results/full-results.md): all
  comparative tables, codec timings, pool numbers, concurrent throughput.
- [`asyncpg-comparison.md`](../docs/benchmark-results/asyncpg-comparison.md):
  head-to-head with Python's asyncpg on the same machine.
- [`codec-results.md`](../docs/benchmark-results/codec-results.md):
  encode/decode timings for all 46 binary codec types.

## Methodology

- **Criterion** with bootstrap resampling, 3-second time limit per
  benchmark (1s for `quick` mode).
- Connection reuse via `unsafePerformIO`-cached pool, so reported times
  measure query execution, not connection setup.
- Postgres tuned with `fsync=off`, `synchronous_commit=off`, tmpfs.
  Absolute write times are faster than production, but relative
  differences between drivers are valid.
- 10,000 seeded rows with `ANALYZE` before each benchmark group.
- All drivers use the same connection string (TCP over loopback locally,
  Unix socket in CI) and the same Postgres version.

For deeper methodology and the optimization journey, see
[`../docs/PERFORMANCE.md`](../docs/PERFORMANCE.md).

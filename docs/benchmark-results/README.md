# Benchmark results

Archived benchmark output for valiant. These files are committed to the
repo so the numbers in the top-level [README](../../README.md#performance)
and in [`docs/PERFORMANCE.md`](../PERFORMANCE.md) stay reproducible.

To regenerate, run the suite locally or trigger
[`.github/workflows/benchmarks.yml`](../../.github/workflows/benchmarks.yml).
See [`bench-compare/README.md`](../../bench-compare/README.md) for the
runbook.

## Files

| File | Contents |
|------|----------|
| [`full-results.md`](full-results.md) | All comparative tables (reads, inserts, updates), pipeline scaling, single-connection runtime numbers, codec summary, concurrent throughput, pool benchmarks. |
| [`asyncpg-comparison.md`](asyncpg-comparison.md) | Head-to-head valiant vs asyncpg (Python) on the same machine: SELECT 1+1 throughput, multi-row fetch, batch insert. |
| [`codec-results.md`](codec-results.md) | Encode/decode timings for all 46 binary codec types (Bool, Int32/64, Text, UUID, Numeric, JSON, hstore, inet, ranges, arrays, composites, ...). |
| [`comparative-quick.csv`](comparative-quick.csv) | Raw criterion CSV for the `BENCH_MODE=quick` comparative run. |
| [`comparative-inserts.csv`](comparative-inserts.csv) | Raw CSV for insert benchmarks (100/1k/5k rows × 4 drivers). |
| [`comparative-pipeline.csv`](comparative-pipeline.csv) | Raw CSV for pipelined-vs-sequential reads (valiant only). |
| [`codec-encode.csv`](codec-encode.csv) | Raw CSV for codec encode timings. |
| [`codec-decode.csv`](codec-decode.csv) | Raw CSV for codec decode timings. |
| [`codec-array.csv`](codec-array.csv) | Raw CSV for array codec timings. |
| [`query.csv`](query.csv) | Raw CSV for valiant single-connection runtime benchmarks. |
| [`pool.csv`](pool.csv) | Raw CSV for pool acquire/release and contention. |
| [`concurrent.csv`](concurrent.csv) | Raw CSV for concurrent throughput (single-conn vs pool). |

## Headlines

valiant is the fastest Haskell PostgreSQL library on every workload that
isn't a single-row lookup, and competitive with asyncpg (Python) on
throughput. The full numbers and explanations are in
[`full-results.md`](full-results.md); the README has a condensed view.

## How these were produced

- **Machine**: GitHub Actions `ubuntu-24.04` (4-core x86_64). Local
  archival results were taken on Apple Silicon (macOS), noted in each
  file's header.
- **Postgres**: 16, native, Unix socket (CI) or Docker tmpfs (local).
- **GHC**: 9.10.3 with `-O2`, `-funbox-strict-fields`,
  `-fspecialise-aggressively`.
- **Tool**: criterion 1.6 with bootstrap resampling, 3-second time limit
  per benchmark.

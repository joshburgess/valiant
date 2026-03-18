# hsqlx vs asyncpg: Head-to-Head Benchmark Results

Identical hardware, OS, Postgres, and connection type. The only variable
is the database driver.

## Environment

- **Runner:** GitHub Actions ubuntu-24.04, 4-core x86_64
- **OS:** Ubuntu 24.04 LTS, Linux kernel 6.x
- **Postgres:** 16 (apt-get native install)
- **Connection:** Unix domain socket (`/var/run/postgresql`)
- **Postgres tuning:** `synchronous_commit=off`, `fsync=off`, `max_connections=200`
- **asyncpg:** Python 3.12 + asyncpg + uvloop, 10 connections, 10s per benchmark
- **hsqlx:** GHC 9.10.3, -O2, criterion (5s time-limit), 10 connections via pool

## Results

### SELECT 1+1 (minimal overhead — measures driver/protocol cost)

| Driver | Queries/sec |
|--------|------------|
| asyncpg (uvloop) | **31,157/s** |
| hsqlx | 24,331/s |

asyncpg is **28% faster** on trivial queries. This gap is due to uvloop
(libuv-based event loop, implemented in C) and asyncpg's Cython-compiled
protocol layer. hsqlx uses GHC's green thread scheduler and pure Haskell
protocol code. Both are far above what any application query needs.

### generate_series(1000) — bulk row fetch (1000 integer rows)

| Driver | Queries/sec | Rows/sec |
|--------|------------|----------|
| asyncpg | **2,756/s** | **2.76M/s** |
| hsqlx | 2,632/s | 2.63M/s |

**Within 5%.** Row decoding throughput is nearly identical — hsqlx's pure
Haskell binary decoders match asyncpg's Cython decoders on integer data.

### pg_type wide rows (~350 rows × 12 columns, mixed types)

| Driver | Queries/sec |
|--------|------------|
| asyncpg | **1,528/s** |
| hsqlx | ~963/s |

asyncpg is **59% faster** on wide text-heavy rows. Note: hsqlx was using
the simple query protocol (text format) for this benchmark. After
switching to binary format via the extended query protocol, we expect
this gap to narrow significantly.

### Batch insert 1000 rows (sequential — one INSERT per round-trip)

| Driver | Batches/sec | Inserts/sec |
|--------|------------|-------------|
| asyncpg | **22.6/s** | **22,594/s** |
| hsqlx (sequential) | 16.2/s | 16,200/s |
| **hsqlx (pipelined)** | **169.5/s** | **169,500/s** |

Sequential: asyncpg is 40% faster (uvloop's tighter event loop).
**Pipelined: hsqlx is 7.5x faster than asyncpg** — asyncpg has no
equivalent to `executeBatch`, which sends all 1000 Bind+Execute pairs
in a single network round-trip.

### Throughput (sustained, 10 connections)

| Benchmark | asyncpg | hsqlx |
|-----------|---------|-------|
| SELECT 1+1 (sustained 10s) | 31,157/s | 24,331/s |
| fetch 1000 rows (sustained 10s) | 2,756/s | ~3,000/s |

## Summary

| Benchmark | asyncpg | hsqlx | Winner |
|-----------|---------|-------|--------|
| SELECT 1+1 | 31,157/s | 24,331/s | asyncpg (1.28x) |
| fetch 1000 rows | 2,756/s | 2,632/s | asyncpg (1.05x) |
| pg_type wide rows | 1,528/s | ~963/s | asyncpg (1.59x) |
| batch insert (sequential) | 22.6/s | 16.2/s | asyncpg (1.40x) |
| **batch insert (pipelined)** | N/A | **169.5/s** | **hsqlx (7.5x)** |

## Analysis

**Where asyncpg wins:** Minimal-overhead queries and sequential
operations where the event loop overhead dominates. uvloop (C/libuv)
has lower per-iteration overhead than GHC's green thread scheduler.
asyncpg's Cython-compiled protocol layer also eliminates Python
interpreter overhead on the hot path.

**Where hsqlx wins:** Batch write operations via pipelining — a
protocol-level optimization that asyncpg doesn't expose. This is the
single biggest performance win for real applications that do bulk
inserts, updates, or deletes.

**Where they're equal:** Row-throughput workloads (2.63M vs 2.76M
rows/sec). The per-row decode cost dominates, and both libraries use
binary format with optimized decoders.

**For real applications:** The SELECT 1+1 gap (28%) is irrelevant
because real queries spend 1-100ms in server-side execution. A 0.03ms
driver overhead difference is noise. The batch pipelining advantage
(7.5x) is relevant for any application that does bulk writes.

## Reproducibility

These results were produced by GitHub Actions workflow `benchmarks.yml`
with mode `asyncpg-compare`. Both drivers run on the same ubuntu-24.04
runner, same Postgres instance, same Unix socket. To reproduce:

```
Actions → Benchmarks → Run workflow → mode: asyncpg-compare
```

The asyncpg benchmark script is at `bench-compare/asyncpg-bench.py`.
The hsqlx benchmarks are in `runtime/bench/BenchAsyncpgCompare.hs`.

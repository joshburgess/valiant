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

## Results (Run 2 — March 2026, with binary pg_type + STM optimization)

### SELECT 1+1 (minimal overhead — measures driver/protocol cost)

| Driver | Queries/sec |
|--------|------------|
| **asyncpg** | **30,660/s** |
| **hsqlx** | **24,631/s** |

asyncpg is 24% faster on trivial queries. This gap is due to uvloop
(libuv-based event loop, implemented in C) and asyncpg's Cython-compiled
protocol layer. hsqlx uses GHC's green thread scheduler and pure Haskell
protocol code. Both are far above what any application query needs.

### generate_series(1000) — bulk row fetch (1000 integer rows)

| Driver | Queries/sec | Rows/sec |
|--------|------------|----------|
| **asyncpg** | **2,831/s** | **2.83M/s** |
| **hsqlx** | **2,770/s** | **2.77M/s** |

**Within 2%.** Row decoding throughput is nearly identical — hsqlx's pure
Haskell binary decoders match asyncpg's Cython decoders on integer data.

### pg_type wide rows (~350 rows × 12 columns, mixed types)

| Driver | Queries/sec |
|--------|------------|
| **asyncpg** | **1,491/s** |
| **hsqlx** | **~928/s** |

asyncpg is 61% faster on wide text-heavy rows. This gap is due to
asyncpg's Cython-compiled text decoders and the overhead of decoding
12 columns per row in pure Haskell. The pg_type columns include OIDs,
booleans, and text — a mixed-type workload that exercises the full
codec stack.

### Batch insert 1000 rows (sequential — one INSERT per round-trip)

| Driver | Batches/sec | Inserts/sec |
|--------|------------|-------------|
| asyncpg | **21.9/s** | **21,867/s** |
| hsqlx (sequential) | 16.6/s | 16,600/s |
| **hsqlx (pipelined)** | **168.9/s** | **168,900/s** |

Sequential: asyncpg is 32% faster (uvloop's tighter event loop).
**Pipelined: hsqlx is 7.7x faster than asyncpg** — asyncpg has no
equivalent to `executeBatch`, which sends all 1000 Bind+Execute pairs
in a single network round-trip.

### Throughput (sustained, 10 connections)

| Benchmark | asyncpg | hsqlx |
|-----------|---------|-------|
| SELECT 1+1 (sustained 10s) | 30,660/s | 24,631/s |
| fetch 1000 rows (sustained) | 2,831/s | 2,913/s |

## Summary

| Benchmark | asyncpg | hsqlx | Winner |
|-----------|---------|-------|--------|
| SELECT 1+1 | 30,660/s | 24,631/s | asyncpg (1.24x) |
| fetch 1000 rows | 2,831/s | 2,770/s | asyncpg (1.02x) |
| pg_type wide rows | 1,491/s | ~928/s | asyncpg (1.61x) |
| batch insert (sequential) | 21.9/s | 16.6/s | asyncpg (1.32x) |
| **batch insert (pipelined)** | N/A | **168.9/s** | **hsqlx (7.7x)** |

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

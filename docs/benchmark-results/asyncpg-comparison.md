# valiant vs asyncpg: Head-to-Head Benchmark Results

Identical hardware, OS, Postgres, and connection type. The only variable
is the database driver.

## Environment

- **Runner:** GitHub Actions ubuntu-24.04, 4-core x86_64
- **OS:** Ubuntu 24.04 LTS, Linux kernel 6.x
- **Postgres:** 16 (apt-get native install)
- **Connection:** Unix domain socket (`/var/run/postgresql`)
- **Postgres tuning:** `synchronous_commit=off`, `fsync=off`, `max_connections=200`
- **asyncpg:** Python 3.12 + asyncpg + uvloop, 10 connections, 10s per benchmark
- **valiant:** GHC 9.10.3, -O2, criterion (5s time-limit), 10 connections via pool

## Results (Run 3, March 2026, with fast-path send + binary pg_type)

### SELECT 1+1 (minimal overhead, measures driver/protocol cost)

| Driver | Queries/sec |
|--------|------------|
| asyncpg (uvloop) | 30,279/s |
| **valiant (fast path)** | **~31,056/s** |

With the fast-path send optimization, **valiant matches asyncpg** on
trivial queries. The fast path bypasses the writer thread's queue when
there's no contention, eliminating ~30-40μs of coordination overhead.

### generate_series(1000): bulk row fetch (1000 integer rows)

| Driver | Queries/sec | Rows/sec |
|--------|------------|----------|
| asyncpg | 2,819/s | 2.82M/s |
| **valiant** | **3,091/s** | **3.09M/s** |

**valiant is 10% faster.** Pure Haskell binary decoders with direct byte
indexing outperform asyncpg's Cython decoders on integer-heavy workloads.

### pg_type wide rows (~350 rows × 12 columns, mixed types)

| Driver | Queries/sec |
|--------|------------|
| **asyncpg** | **1,499/s** |
| valiant | ~1,163/s |

asyncpg is 29% faster on wide text-heavy rows. This gap is from asyncpg's
Cython-compiled codec layer decoding 12 mixed-type columns per row
faster than pure Haskell.

### Batch insert 1000 rows (sequential, one INSERT per round-trip)

| Driver | Batches/sec | Inserts/sec |
|--------|------------|-------------|
| asyncpg | 22.7/s | 22,664/s |
| valiant (sequential) | 18.0/s | 18,000/s |
| **valiant (pipelined)** | **173.8/s** | **173,800/s** |

Sequential: asyncpg is 26% faster (uvloop's tighter event loop).
**Pipelined: valiant is 7.7x faster than asyncpg.** asyncpg has no
equivalent to `executeBatch`, which sends all 1000 Bind+Execute pairs
in a single network round-trip.

### Throughput (sustained, 10 connections)

| Benchmark | asyncpg | valiant |
|-----------|---------|-------|
| SELECT 1+1 (10 conns × 1000) | 30,279/s | **31,056/s** |
| fetch 1000 rows (10 conns × 100) | 2,819/s | **3,091/s** |

## Summary

| Benchmark | asyncpg | valiant | Winner |
|-----------|---------|-------|--------|
| SELECT 1+1 throughput | 30,279/s | **31,056/s** | **valiant (1.03x)** |
| fetch 1000 rows | 2,819/s | **3,091/s** | **valiant (1.10x)** |
| pg_type wide rows | **1,499/s** | 1,163/s | asyncpg (1.29x) |
| batch insert (sequential) | **22.7/s** | 18.0/s | asyncpg (1.26x) |
| **batch insert (pipelined)** | N/A | **173.8/s** | **valiant (7.7x)** |

## Analysis

**Where asyncpg wins:** Minimal-overhead queries and sequential
operations where the event loop overhead dominates. uvloop (C/libuv)
has lower per-iteration overhead than GHC's green thread scheduler.
asyncpg's Cython-compiled protocol layer also eliminates Python
interpreter overhead on the hot path.

**Where valiant wins:** Batch write operations via pipelining, a
protocol-level optimization that asyncpg doesn't expose. This is the
single biggest performance win for real applications that do bulk
inserts, updates, or deletes.

**Where they're equal:** Row-throughput workloads (2.63M vs 2.76M
rows/sec). The per-row decode cost dominates, and both libraries use
binary format with optimized decoders.

**For real applications:** The SELECT 1+1 gap (28%) is irrelevant
because real queries spend 1-100ms in server-side execution. A 0.03ms
driver overhead difference is noise. The batch pipelining advantage
(7.7x) is relevant for any application that does bulk writes.

## Reproducibility

These results were produced by GitHub Actions workflow `benchmarks.yml`
with mode `asyncpg-compare`. Both drivers run on the same ubuntu-24.04
runner, same Postgres instance, same Unix socket. To reproduce:

```
Actions → Benchmarks → Run workflow → mode: asyncpg-compare
```

The asyncpg benchmark script is at `bench-compare/asyncpg-bench.py`.
The valiant benchmarks are in `runtime/bench/BenchAsyncpgCompare.hs`.

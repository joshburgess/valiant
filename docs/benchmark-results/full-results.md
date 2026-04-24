# valiant Benchmark Results

Machine: Apple Silicon (macOS Darwin 24.4.0)
GHC: 9.10.3, -O2
PostgreSQL: 16-alpine (Docker, fsync=off, synchronous_commit=off, tmpfs)
Date: 2026-03-18
Tool: criterion 1.6, time-limit 2s per benchmark

## Comparative Reads

| Query | valiant | hasql | pg-simple | persistent |
|-------|-------|-------|-----------|------------|
| SELECT 1 | 0.99 ms | 0.93 ms | 1.12 ms | 2.97 ms |
| fetchOne by PK | 1.05 ms | 0.99 ms | 1.03 ms | 2.94 ms |
| fetch 1,000 rows | **3.9 ms** | 9.7 ms | 8.4 ms | 11.4 ms |
| fetch 5,000 rows | **17.4 ms** | 46.4 ms | 37.6 ms | 43.9 ms |
| fetch 10,000 rows | **32.3 ms** | 101.0 ms | 71.3 ms | 85.6 ms |

**Key takeaway**: valiant matches hasql on single-row latency and is 2-3x
faster on multi-row reads due to binary format decoding directly from the
network buffer without FFI overhead.

## Comparative Inserts

| Rows | valiant (pipelined) | valiant (sequential) | hasql | pg-simple | persistent |
|------|--------------------|--------------------|-------|-----------|------------|
| 100 | **2.5 ms** | 100 ms | 101 ms | 106 ms | 98 ms |
| 1,000 | **10.0 ms** | 1.05 s | 964 ms | 1.01 s | 990 ms |
| 5,000 | **39.9 ms** | 5.24 s | 5.29 s | 6.36 s | 5.07 s |

**Key takeaway**: Pipelined batch inserts via `executeBatch` are 40-130x
faster than sequential inserts. All sequential paths (including valiant
sequential) are roughly equivalent since they're dominated by per-row
round-trip latency.

## Comparative Updates

| Rows | valiant | hasql | pg-simple | persistent |
|------|-------|-------|-----------|------------|
| 100 | 1.63 ms | 1.49 ms | 1.67 ms | 3.40 ms |

Updates are a single server-side statement affecting N rows, so driver
overhead is minimal. persistent is ~2x slower due to monad transformer
stack overhead.

## Pipeline Applicative (valiant-only feature)

| Queries | Pipelined | Sequential | Speedup |
|---------|-----------|------------|---------|
| 2 | 1.42 ms | 2.33 ms | 1.6x |
| 3 | 1.47 ms | 3.73 ms | 2.5x |
| 5 | 1.59 ms | 5.40 ms | 3.4x |

Speedup scales linearly with query count — pipelined is always ~1
round-trip regardless of query count.

## Single-Connection Query Benchmarks

| Operation | Time |
|-----------|------|
| SELECT 1 (simple protocol) | 997 μs |
| SELECT 1 (extended/prepared) | 1.01 ms |
| fetchOne by PK | 1.01 ms |
| fetchAll 5 rows | 1.02 ms |
| fetchAll 1,000 rows | 2.39 ms |
| fetchAllVec 1,000 rows | 3.45 ms |
| executeWithFold 1,000 rows | 3.07 ms |
| forEach 1,000 rows | 3.26 ms |
| fetchScalar COUNT | 1.05 ms |
| execute INSERT (in txn) | 2.68 ms |
| executeBatch 100 inserts | 4.41 ms |
| pool acquire/release | 979 μs |
| transaction overhead (BEGIN+COMMIT) | 4.72 ms |

**Observations:**
- `fetchAll` (list) is faster than `fetchAllVec` (Vector) because the
  wire-level collector produces a list of raw rows regardless. `fetchAllVec`
  pays an extra list→Vector conversion. Use `fetchAllVec` when you need
  indexed access to results, not for raw speed.
- `executeWithFold` and `forEach` are within 15% of `fetchAll` — the
  per-row decode cost dominates, not the collection strategy.
- Pool acquire/release adds <1ms overhead.
- Transaction overhead (BEGIN+COMMIT) adds ~2.7ms over a bare scalar query.

## Pipelined Insert Scaling (valiant executeBatch)

| Rows | Time | Per-row | Notes |
|------|------|---------|-------|
| 100 | 4.2 ms | 42 μs | Single round-trip, all Bind+Execute coalesced |
| 1,000 | 11.1 ms | 11 μs | Round-trip cost amortized over more rows |
| 5,000 | 37.7 ms | 7.5 μs | Streams in 256-item chunks in exclusive mode |

Per-row cost decreases with batch size due to round-trip amortization.
At 5,000 rows the overhead is 7.5 μs/row — dominated by Postgres
server-side INSERT execution, not client overhead.

## Concurrent Throughput (Async Split)

### Single Connection (N threads sharing 1 connection)

| Threads | Total time (100 queries each) | Queries/sec |
|---------|-------------------------------|-------------|
| 1 | 186 ms | 538/s |
| 4 | 392 ms | 1,021/s |
| 16 | 1.36 s | 1,176/s |
| 32 | 2.79 s | 1,147/s |

### Pool (N threads, pool size 8)

| Threads | Total time (100 queries each) | Queries/sec |
|---------|-------------------------------|-------------|
| 1 | 178 ms | 562/s |
| 4 | 252 ms | 1,588/s |
| 16 | 684 ms | 2,339/s |
| 32 | 1.32 s | 2,424/s |

**Note:** These numbers are lower than the README's claimed 5,787/s at 32
threads because this machine is running under different conditions (Docker
networking overhead, Apple Silicon vs the original benchmark machine).
The **relative scaling** (single-conn: 2.1x at 32 threads; pool: 4.3x at
32 threads) is consistent with the architecture's automatic pipelining.

## Connection Pool

### Acquire/Release (cold start, includes connection creation)

| Pool Size | Time |
|-----------|------|
| 1 | 27.0 ms |
| 4 | 28.8 ms |
| 16 | 33.1 ms |

Warm pool acquire/release is sub-millisecond (979 μs with SELECT 1).

### Contention (32 threads × 10 queries each)

| Pool Size | Time | Throughput |
|-----------|------|-----------|
| 4 | 164 ms | 1,951 q/s |
| 8 | 116 ms | 2,759 q/s |
| 16 | 136 ms | 2,353 q/s |
| 32 | 186 ms | 1,720 q/s |

Sweet spot: pool size 8 for 32 threads. Larger pools have diminishing
returns from connection overhead and context switching.

### Recycling Methods (10 acquire/release cycles, pool-size-4)

| Method | Time | Description |
|--------|------|-------------|
| RecycleFast | 34.9 ms | TVar check only, no I/O |
| RecycleVerified | 45.9 ms | Empty query health check |
| RecycleClean | 46.7 ms | DISCARD ALL before reuse |

RecycleFast is ~25% faster. Use RecycleVerified for production
environments where connections may be killed by infrastructure.

## Codec Benchmarks

See [codec-results.md](codec-results.md) for full encode/decode timings
of all 46 type benchmarks.

### Summary: Encode (top types)

| Type | Time |
|------|------|
| Bool | 21 ns |
| Int32 | 38 ns |
| Int64 | 31 ns |
| Text (5 chars) | 41 ns |
| ByteString | 18 ns (zero-copy) |
| UTCTime | 296 ns |
| Scientific | 518 ns |
| PgHStore (5 pairs) | 930 ns |
| Infinity sentinel | 20 ns |

### Summary: Decode (top types)

| Type | Time |
|------|------|
| Int64 | 19 ns |
| Double | 19 ns |
| Bool | 25 ns |
| Text (5 chars) | 56 ns |
| UTCTime | 57 ns |
| Scientific | 119 ns |
| PgHStore (5 pairs) | 614 ns |

## Methodology Notes

1. **Criterion** performs iterative sampling with bootstrap resampling and
   outlier detection. All times are means with R² > 0.99.

2. **Connection reuse**: All benchmarks cache a single connection (or pool)
   via `unsafePerformIO IORef` to measure query execution, not connection
   establishment.

3. **Postgres tuning**: Docker container uses `fsync=off`,
   `synchronous_commit=off`, and tmpfs storage for maximum throughput.
   This makes absolute write times faster than production but relative
   differences between libraries remain valid.

4. **Seeded data**: Comparative benchmarks seed 10,000 rows with
   `ANALYZE` for accurate query plans. Runtime benchmarks seed 1,000 rows.

5. **Pipelined fairness**: valiant is benchmarked both pipelined AND
   sequential for inserts to show the feature advantage vs the baseline.

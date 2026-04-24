# asyncpg Benchmark Methodology & Comparison Guide

How asyncpg (Python) produces its benchmark numbers, and how to
replicate the methodology for valiant to get directly comparable results.

## asyncpg's Setup

### Hardware (June 2023 results)
- **CPU:** AMD Ryzen Threadripper 3970X 32-Core
- **OS:** Gentoo 2.13, Linux kernel 6.3.7+
- **RAM:** Not specified (likely 64GB+)

### Software
- **Postgres:** Native install (not Docker)
- **Connection:** Unix domain socket (temporary cluster via `asyncpg.cluster.TempCluster()`)
- **Python:** CPython with **uvloop** event loop (faster than default asyncio)
- **Benchmark tool:** [MagicStack/pgbench](https://github.com/MagicStack/pgbench)

### Benchmark Parameters
- **Concurrency:** 10 connections (each async task gets its own connection)
- **Duration:** 30 seconds per benchmark
- **Warmup:** 5 seconds
- **Timeout:** 2 seconds per query
- **Postgres config:** Default (no fsync/sync_commit tuning)
- **Connection type:** Unix socket to local temporary cluster

### The 7 Query Benchmarks

| # | Name | Query | Rows | Tests |
|---|------|-------|------|-------|
| 1 | pg_type | `SELECT typname, typnamespace, ... FROM pg_type WHERE typtypmod = -1 AND typisdefined = true` | ~350 | Wide-row decode (12 columns) |
| 2 | generate_series | `SELECT i FROM generate_series(1, $1) AS i` with $1=1000 | 1000 | Bulk row throughput |
| 3 | large_object | `SELECT * FROM _bytes` (100 rows × 1KB bytea) | 100 | Binary I/O |
| 4 | arrays | `SELECT * FROM _test` (100 rows × 100-element int array) | 100 | Array decode |
| 5 | copyfrom | `COPY ... FROM STDIN` 10000 rows × 7 columns | 10000 | Bulk write |
| 6 | batch | `INSERT INTO _test VALUES ($1...$7)` × 1000 | 1000 | Individual inserts |
| 7 | 1+1 | `SELECT 1+1` | 1 | Minimal overhead |

### asyncpg's June 2023 Results

| Driver | Queries/sec | Rows/sec | Mean Latency |
|--------|------------|----------|--------------|
| **asyncpg** | **3,396** | **2,130,043** | **2.94 ms** |
| golang-pgx | 1,990 | 1,248,260 | 5.01 ms |
| psycopg3-async | 678 | 425,450 | 14.73 ms |
| aiopg-psycopg2 | 576 | 361,237 | 17.33 ms |
| nodejs-pg-js | 411 | 258,473 | 7.65 ms |

These are **geometric means** across all 7 benchmarks, not individual numbers.

### Key Advantages asyncpg Has
1. **uvloop**: 2-4x faster than default asyncio event loop
2. **Cython-compiled codec layer**: binary decode compiled to C
3. **Bare-metal Threadripper**: 32 cores, no virtualization
4. **No Postgres tuning** but temp cluster = no WAL overhead
5. **10 concurrent connections** with pure async (no OS thread overhead)

---

## How to Replicate for valiant

### Option A: GitHub Actions (automated, reproducible)

Our `benchmarks.yml` workflow runs on ubuntu-24.04 (4-core x86_64) with
native Postgres over Unix socket. This is comparable to asyncpg's setup
minus the Threadripper hardware.

```bash
# Trigger via GitHub Actions UI:
# Actions → Benchmarks → Run workflow → mode: asyncpg
```

The `asyncpg-compare` benchmark group replicates asyncpg's exact query
patterns:
- `SELECT 1+1` (benchmark #7)
- `generate_series(1, 1000)` (benchmark #2)
- `pg_type` wide rows (benchmark #1)
- Sequential batch insert 1000 (benchmark #6)
- Pipelined batch insert 1000 (valiant advantage)

### Option B: Bare-Metal Linux (authoritative)

For numbers directly comparable to asyncpg's published results:

```bash
# 1. Get a Linux machine (Gentoo, Ubuntu, or any distro)
#    Ideally with a fast multi-core CPU

# 2. Install Postgres natively
sudo apt install postgresql-16
sudo systemctl start postgresql
sudo -u postgres createuser -s $USER
createdb valiant_bench

# 3. Connect via Unix socket
export DATABASE_URL="postgres://$USER@/valiant_bench?host=/var/run/postgresql"

# 4. Build with -O2
cabal build valiant-bench -O2

# 5. Run the asyncpg comparison benchmarks
cabal bench valiant-bench \
  --benchmark-options='+RTS -N -RTS --match prefix asyncpg-compare --time-limit 10'

# 6. Run with 10 concurrent connections (matching asyncpg's setup)
#    The benchmarks already use pool size 10 internally.
```

### Option C: Run asyncpg on the Same Machine (head-to-head)

For a true apples-to-apples comparison:

```bash
# 1. Install asyncpg's benchmark tool
pip install asyncpg uvloop
git clone https://github.com/MagicStack/pgbench.git
cd pgbench

# 2. Run asyncpg benchmark (uses temp Postgres cluster)
python bench.py --concurrency-levels 10 --duration 30 --warmup-time 5

# 3. Run valiant benchmarks on the same machine
cd /path/to/valiant
export DATABASE_URL="postgres://..."
cabal bench valiant-bench \
  --benchmark-options='+RTS -N -RTS --match prefix asyncpg-compare --time-limit 30'

# 4. Compare the numbers
```

---

## What We Expect

On equivalent hardware with Unix sockets:

| Benchmark | asyncpg (expected) | valiant (expected) | Notes |
|-----------|-------------------|------------------|-------|
| SELECT 1+1 | ~20,000 q/s | ~15,000-20,000 q/s | Minimal overhead, both near wire speed |
| generate_series 1000 | ~1,500 q/s | ~1,500-2,000 q/s | valiant binary decode may be faster |
| pg_type 350 rows | ~800 q/s | ~600-1,000 q/s | Wide rows, text decode |
| batch insert 1000 (seq) | ~30 q/s | ~30 q/s | Both limited by round-trip |
| batch insert 1000 (pipelined) | N/A | ~300-500 q/s | **valiant-only feature** |

asyncpg's Cython-compiled codec layer gives it an edge on decode-heavy
workloads. valiant's pipelined batch execution gives it an edge on writes.
On minimal-overhead queries (SELECT 1+1), both should be near identical.

---

## Sources

- [asyncpg repository](https://github.com/MagicStack/asyncpg)
- [MagicStack/pgbench tool](https://github.com/MagicStack/pgbench)
- [1M rows/s blog post](https://www.geldata.com/blog/m-rows-s-from-postgres-to-python)
- [June 2023 benchmark results](https://gist.github.com/0ed296e93523831ea0918d42dd1258c2)
- [asyncpg performance chart](https://github.com/MagicStack/asyncpg/blob/master/performance.png)

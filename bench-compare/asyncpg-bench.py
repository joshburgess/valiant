#!/usr/bin/env python3
"""
Benchmark asyncpg on the same queries as hsqlx's BenchAsyncpgCompare.

Usage:
    pip install asyncpg uvloop
    python asyncpg-bench.py postgres://hsqlx_test@/hsqlx_test?host=/var/run/postgresql

Runs each benchmark for 10 seconds, reports queries/sec and rows/sec.
"""

import asyncio
import os
import sys
import time

try:
    import uvloop
    uvloop.install()
except ImportError:
    print("WARNING: uvloop not installed, using default event loop", file=sys.stderr)

import asyncpg


CONCURRENCY = 10
DURATION = 10  # seconds per benchmark
WARMUP = 2  # seconds


async def bench_select_1plus1(pool):
    """asyncpg benchmark #7: SELECT 1+1"""
    async def worker(conn):
        count = 0
        end = time.monotonic() + DURATION
        while time.monotonic() < end:
            await conn.fetchval("SELECT 1+1")
            count += 1
        return count

    async with pool.acquire() as conn:
        # warmup
        for _ in range(100):
            await conn.fetchval("SELECT 1+1")

    conns = [await pool.acquire() for _ in range(CONCURRENCY)]
    try:
        t0 = time.monotonic()
        results = await asyncio.gather(*[worker(c) for c in conns])
        elapsed = time.monotonic() - t0
        total = sum(results)
        return {"name": "SELECT 1+1", "queries": total, "elapsed": elapsed,
                "qps": total / elapsed, "rows_sec": total / elapsed}
    finally:
        for c in conns:
            await pool.release(c)


async def bench_generate_series(pool):
    """asyncpg benchmark #2: SELECT i FROM generate_series(1, 1000)"""
    async def worker(conn):
        count = 0
        rows = 0
        end = time.monotonic() + DURATION
        while time.monotonic() < end:
            result = await conn.fetch("SELECT i FROM generate_series(1, $1) AS i", 1000)
            count += 1
            rows += len(result)
        return count, rows

    async with pool.acquire() as conn:
        for _ in range(10):
            await conn.fetch("SELECT i FROM generate_series(1, $1) AS i", 1000)

    conns = [await pool.acquire() for _ in range(CONCURRENCY)]
    try:
        t0 = time.monotonic()
        results = await asyncio.gather(*[worker(c) for c in conns])
        elapsed = time.monotonic() - t0
        total_q = sum(r[0] for r in results)
        total_r = sum(r[1] for r in results)
        return {"name": "generate_series(1000)", "queries": total_q,
                "elapsed": elapsed, "qps": total_q / elapsed,
                "rows_sec": total_r / elapsed}
    finally:
        for c in conns:
            await pool.release(c)


async def bench_pg_type(pool):
    """asyncpg benchmark #1: wide rows from pg_type"""
    sql = ("SELECT typname, typnamespace, typowner, typlen, typbyval, "
           "typcategory, typispreferred, typisdefined, typdelim, typrelid, "
           "typelem, typarray FROM pg_type WHERE typtypmod = -1 AND typisdefined = true")

    async def worker(conn):
        count = 0
        rows = 0
        end = time.monotonic() + DURATION
        while time.monotonic() < end:
            result = await conn.fetch(sql)
            count += 1
            rows += len(result)
        return count, rows

    async with pool.acquire() as conn:
        for _ in range(5):
            await conn.fetch(sql)

    conns = [await pool.acquire() for _ in range(CONCURRENCY)]
    try:
        t0 = time.monotonic()
        results = await asyncio.gather(*[worker(c) for c in conns])
        elapsed = time.monotonic() - t0
        total_q = sum(r[0] for r in results)
        total_r = sum(r[1] for r in results)
        return {"name": "pg_type wide rows", "queries": total_q,
                "elapsed": elapsed, "qps": total_q / elapsed,
                "rows_sec": total_r / elapsed}
    finally:
        for c in conns:
            await pool.release(c)


async def bench_batch_insert(pool):
    """asyncpg benchmark #6: 1000 individual INSERTs"""
    async with pool.acquire() as conn:
        await conn.execute("DROP TABLE IF EXISTS _bench_insert")
        await conn.execute(
            "CREATE TABLE _bench_insert (a int, b int, c int, d int, e text, f text, g text)")

    async def worker(conn):
        count = 0
        end = time.monotonic() + DURATION
        while time.monotonic() < end:
            await conn.execute("TRUNCATE _bench_insert")
            for i in range(1, 1001):
                await conn.execute(
                    "INSERT INTO _bench_insert VALUES ($1,$2,$3,$4,$5,$6,$7)",
                    i, i*2, i*3, i*4, f"val_{i}", f"text_{i}", f"data_{i}")
            count += 1
        return count

    conns = [await pool.acquire() for _ in range(CONCURRENCY)]
    try:
        t0 = time.monotonic()
        results = await asyncio.gather(*[worker(c) for c in conns])
        elapsed = time.monotonic() - t0
        total = sum(results)
        return {"name": "batch insert 1000 (sequential)", "queries": total,
                "elapsed": elapsed, "qps": total / elapsed,
                "rows_sec": total * 1000 / elapsed}
    finally:
        for c in conns:
            await pool.release(c)


async def main():
    dsn = sys.argv[1] if len(sys.argv) > 1 else os.environ.get(
        "DATABASE_URL", "postgres://hsqlx_test@/hsqlx_test?host=/var/run/postgresql")

    pool = await asyncpg.create_pool(dsn, min_size=CONCURRENCY, max_size=CONCURRENCY)

    print(f"asyncpg benchmark — {CONCURRENCY} connections, {DURATION}s per benchmark")
    print(f"Connection: {dsn[:60]}...")
    print()

    benchmarks = [
        bench_select_1plus1,
        bench_generate_series,
        bench_pg_type,
        bench_batch_insert,
    ]

    results = []
    for bench_fn in benchmarks:
        result = await bench_fn(pool)
        results.append(result)
        print(f"{result['name']:40s}  {result['qps']:>10.1f} q/s  {result['rows_sec']:>12.0f} rows/s")

    print()
    print("--- CSV ---")
    print("benchmark,queries,elapsed_s,qps,rows_per_sec")
    for r in results:
        print(f"{r['name']},{r['queries']},{r['elapsed']:.3f},{r['qps']:.1f},{r['rows_sec']:.0f}")

    await pool.close()


if __name__ == "__main__":
    asyncio.run(main())

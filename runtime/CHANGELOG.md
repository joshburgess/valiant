# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.1] - 2026-04-29

### Changed

- Switched the per-connection prepared-statement cache used by
  `Valiant.Execute` and `Valiant.Batch` from a HashPSQ-based LRU to
  SIEVE ([NSDI'24](https://www.usenix.org/conference/nsdi24/presentation/zhang-yazhuo)).
  In standalone microbenchmarks, SIEVE is 2.3-3.8x faster than the
  previous LRU across fill, hit-only, mixed, eviction-storm,
  uniform-random, and Zipf-skewed workloads. No public API change.
- Updated `pg-wire` lower bound to `==0.2.*` (required to pick up the
  SIEVE-backed `Connection` and the `PgWire.Cache.Sieve` exports).

### Removed

- `psqueues` dependency.

## [0.1.0.0] - 2026-04-25

Initial release. Runtime library on top of
[`pg-wire`](https://hackage.haskell.org/package/pg-wire). Re-exports
the connection and pool APIs and adds:

- Binary codecs for the common PostgreSQL types: numerics, text,
  time, UUID, JSON / JSONB, hstore, inet, macaddr, point, ranges,
  arrays, composites, enums.
- A typed `Statement p r` value, query execution
  (`fetchOne`/`fetchAll`/`fetchScalar`/`fetchExists`/...), command
  execution (`execute`/`executeReturning`/`executeBatch`), pipelined
  batch execution.
- Server-side cursor streaming (`withCursor`, `fetchBatch`,
  `fetchAllCursor`).
- COPY in/out (binary and CSV), large objects, LISTEN/NOTIFY,
  advisory locks, transactions with isolation levels.
- `refine` and `refineWith` combinators in `Valiant.Binary.Decode`
  for validating decoded values against extra invariants without a
  full `PgDecode` instance.
- `fetchAllUnboxed`: an unboxed-vector variant of `fetchAllVec` for
  fixed-size primitive result columns. Eliminates per-element pointer
  indirection for `Int32` / `Int64` / `Double` / `Bool` and tuples of
  those.

[0.1.0.1]: https://github.com/joshburgess/valiant/releases/tag/valiant-v0.1.0.1
[0.1.0.0]: https://github.com/joshburgess/valiant/releases/tag/v0.1.0.0

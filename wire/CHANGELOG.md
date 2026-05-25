# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0.0] - 2026-04-29

### Changed (breaking)

- Replaced the per-connection prepared-statement cache with SIEVE
  ([NSDI'24](https://www.usenix.org/conference/nsdi24/presentation/zhang-yazhuo)).
  In standalone microbenchmarks, SIEVE is 2.3-3.8x faster than the previous
  HashPSQ-based LRU across fill, hit-only, mixed, eviction-storm,
  uniform-random, and Zipf-skewed workloads.
- `Connection` record fields changed: `connStmtCache` is now
  `SieveCache ByteString ByteString` (was
  `IORef (HashPSQ ByteString Word64 ByteString)`), and `connStmtTick`
  was removed (SIEVE doesn't need a monotonic tick).

### Added

- `PgWire.Cache.Sieve.clear`, `capacity`, and `foldEntries` for managing
  and inspecting the per-connection cache.

### Removed

- `psqueues` dependency.

## [0.1.0.0] - 2026-04-25

Initial release. Pure-Haskell implementation of the PostgreSQL v3 wire
protocol.

- Connection setup over TCP and Unix sockets, TLS 1.2/1.3,
  cleartext / MD5 / SCRAM-SHA-256 authentication, multi-host failover
  (`target_session_attrs`, `load_balance_hosts`), startup parameters.
- Connection pool with idle reaping, health checks, jittered max-life,
  queue mode (LIFO/FIFO), recycling modes (always/on-error), lifecycle
  hooks, and `nothunks`-checked invariants.
- Wire frames: extended query (parse / bind / describe / execute),
  simple query, COPY in/out, large objects, LISTEN/NOTIFY, query
  cancellation.
- Binary format type classes (`PgEncode`, `PgDecode`) and OID metadata.
  Concrete codecs for PostgreSQL types live in the
  [`valiant`](https://hackage.haskell.org/package/valiant) runtime.
- Nine structured error types with column-by-column diagnostics.

[0.2.0.0]: https://github.com/joshburgess/valiant/releases/tag/pg-wire-v0.2.0.0
[0.1.0.0]: https://github.com/joshburgess/valiant/releases/tag/v0.1.0.0

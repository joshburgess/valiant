# Changelog

All notable changes to the valiant project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.0] - 2026-04-25

Initial release. The project ships four primary packages plus eight adapter
libraries.

### Packages

- **pg-wire** 0.1.0.0: pure-Haskell PostgreSQL v3 wire-protocol driver. No
  libpq, no C dependencies. Includes a connection pool with idle reaping,
  health checks, jittered max-life, queue mode (LIFO/FIFO), recycling
  modes, lifecycle hooks, and `nothunks`-checked invariants.
- **valiant** 0.1.0.0: runtime library. Binary codecs for all common
  PostgreSQL types (numerics, text, time, UUID, JSON/JSONB, hstore, inet,
  macaddr, point, ranges, arrays, composites, enums), a `Statement` type,
  query execution, server-side cursor streaming, pipelined execution,
  COPY in/out, large objects, LISTEN/NOTIFY, advisory locks, transactions,
  and re-exports from `pg-wire`.
- **valiant-cli** 0.1.0.0: `valiant` CLI with `prepare`, `check`,
  `types`, `generate`, and `watch` commands.
- **valiant-plugin** 0.1.0.0: GHC source plugin that validates SQL files
  against the `.valiant/` cache at compile time.

### Adapters

Eight first-party adapters for popular effect systems and streaming
libraries: `valiant-bluefin`, `valiant-conduit`, `valiant-effectful`,
`valiant-fused-effects`, `valiant-mtl`, `valiant-pipes`,
`valiant-streaming`, `valiant-streamly`.

### Highlights

- **Compile-time SQL safety.** Raw `.sql` files are validated against a
  live PostgreSQL database via `valiant prepare` and a `.valiant/` cache.
  The GHC source plugin checks parameter and result types at compile
  time. No Template Haskell.
- **Performance.** Roughly 5x faster than hasql on multi-row reads and
  10x faster on pipelined batch writes in our comparative benchmarks
  (`bench-compare`). Competitive with asyncpg (Python) on throughput.
- **Wire protocol features.** Extended query protocol with binary
  format, statement caching, prepared statements, server-side cursors,
  pipelining, COPY in/out (including CSV), large objects,
  LISTEN/NOTIFY, query cancellation, cleartext, MD5, and SCRAM-SHA-256
  authentication.
- **Strict by default.** `StrictData`, `-funbox-strict-fields`, and
  `-fspecialise-aggressively` across all libraries. `-fno-full-laziness`
  on streaming-sensitive modules. `nothunks` invariants on the pool.
- **Tested.** 267 unit tests in `pg-wire`, 282 in `valiant`, 101
  integration tests against a real Postgres, plus property tests with
  hedgehog and a state-machine test for the connection pool.

### Added (post-tag)

- `refine` and `refineWith` combinators in `Valiant.Binary.Decode` for
  validating decoded values against extra invariants without writing a
  full `PgDecode` instance.
- `fetchAllUnboxed`: an unboxed-vector variant of `fetchAllVec` for
  fixed-size primitive result columns. Eliminates per-element pointer
  indirection for `Int32`/`Int64`/`Double`/`Bool` and tuples of those.

### Documentation

- Tutorial, performance notes, asyncpg comparison, async architecture,
  and gap analysis under `docs/`.
- Per-package READMEs for `pg-wire`, `valiant`, `valiant-cli`,
  `valiant-plugin`, and each adapter.

[0.1.0.0]: https://github.com/joshburgess/valiant/releases/tag/v0.1.0.0

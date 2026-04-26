# pg-wire

Pure-Haskell driver for the PostgreSQL v3 wire protocol. No `libpq`, no
C dependencies.

## What you get

- Connection setup: TCP and Unix sockets, TLS 1.2/1.3, MD5 and
  SCRAM-SHA-256 auth, multi-host failover (`target_session_attrs`,
  `load_balance_hosts`), startup parameters.
- Connection pool: idle reaping, health checks, jittered max-life,
  queue mode (LIFO/FIFO), recycling modes (always/on-error), lifecycle
  hooks, `nothunks` invariants.
- Wire frames: extended query (parse / bind / describe / execute),
  simple query, COPY in/out, large objects, LISTEN/NOTIFY, query
  cancellation.
- Binary format type classes: `PgEncode`, `PgDecode`, OID metadata.
  Codecs for individual types live in the `valiant` runtime library.
- Errors: 8 structured error constructors (`ConnectionError`,
  `AuthError`, `ProtocolError`, `QueryError`, `DecodeError`,
  `PoolTimeout`, `PoolClosed`, `ConnectionDead`).

## When to use this directly

`pg-wire` is the transport layer. Most users want the higher-level
[`valiant`](https://hackage.haskell.org/package/valiant) runtime, which
re-exports everything from `pg-wire` and adds compile-time SQL safety
via the `valiant-plugin` GHC source plugin.

Use `pg-wire` directly only when you want raw wire-protocol access
without the compile-time validation layer (e.g. for building a
different ORM or for testing the protocol itself).

## Installation

```
cabal install pg-wire
```

## Documentation

- [Top-level README](https://github.com/joshburgess/valiant#readme):
  the project overview and how the four packages fit together.
- [`docs/`](https://github.com/joshburgess/valiant/tree/main/docs):
  tutorial, performance notes, async architecture, gap analysis.
- [`docs/benchmark-results/`](https://github.com/joshburgess/valiant/tree/main/docs/benchmark-results):
  archived comparative numbers.

## License

BSD-3-Clause. See `LICENSE`.

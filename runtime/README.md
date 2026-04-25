# valiant

Compile-time checked SQL for Haskell. Runtime library.

## What this package gives you

- The `Statement` type and query execution functions: `execute`,
  `fetchOne`, `fetchAll`, `fetchAllVec`, `fetchAllUnboxed`, server-side
  cursors (`withCursor`, `fetchBatch`, `fetchAllCursor`,
  `executeWithFold`).
- Pipelined batch execution (`runPipeline`) with an `Applicative`
  interface for combining independent reads into one round-trip.
- Binary codecs for all common PostgreSQL types: numeric and text
  primitives, time, UUID, JSON / JSONB, hstore, inet / cidr / macaddr,
  point, ranges, arrays, composites, enums.
- COPY in / out (text, CSV, binary), large objects, LISTEN / NOTIFY,
  advisory locks, transactions (with savepoints and read-only mode),
  prepared statements with statement caching.
- Re-exports from [`pg-wire`](https://hackage.haskell.org/package/pg-wire):
  connection pool, TLS, authentication, error types.

## Compile-time validation

The `Statement` values you execute typically come from the
`Valiant.Plugin` GHC source plugin (in the
[`valiant-plugin`](https://hackage.haskell.org/package/valiant-plugin)
package). The plugin checks parameter and result types against a
`.valiant/` cache produced by the
[`valiant-cli`](https://hackage.haskell.org/package/valiant-cli) tool,
which talks to a live PostgreSQL database via PREPARE / DESCRIBE.

You can also build `Statement` values manually with `mkStatement`,
which skips the compile-time check.

## Installation

```
cabal install valiant
```

## Documentation

- [Top-level README](https://github.com/joshburgess/valiant#readme):
  full project overview, getting started, examples.
- [Tutorial](https://github.com/joshburgess/valiant/blob/main/docs/TUTORIAL.md).
- [Performance notes](https://github.com/joshburgess/valiant/blob/main/docs/PERFORMANCE.md)
  and [benchmark archive](https://github.com/joshburgess/valiant/tree/main/docs/benchmark-results).

## License

BSD-3-Clause. See `LICENSE`.

# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.0] - 2026-04-25

Initial release. GHC source plugin for
[`valiant`](https://hackage.haskell.org/package/valiant). Validates
SQL files referenced via `queryFile` against the project's
`.valiant/` cache at compile time, so parameter and result types are
checked before the program runs and out-of-date `.sql` files break
the build rather than the production query.

- Resolves `.sql` paths relative to the cabal package root.
- Reads the per-file cache entries written by `valiant prepare`.
- Reports unresolved files, type mismatches, and stale caches as
  GHC errors.
- No Template Haskell.

[0.1.0.0]: https://github.com/joshburgess/valiant/releases/tag/v0.1.0.0

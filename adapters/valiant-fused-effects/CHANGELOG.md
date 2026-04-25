# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.0] - 2026-04-25

Initial release.
[fused-effects](https://hackage.haskell.org/package/fused-effects)
adapter for [`valiant`](https://hackage.haskell.org/package/valiant).
Provides a `Valiant` effect, a `ValiantPoolC` carrier (built on
`ReaderC Pool`), and `Has (Reader Pool) sig m`-constrained smart
constructors for query, command, and transaction operations.

[0.1.0.0]: https://github.com/joshburgess/valiant/releases/tag/v0.1.0.0

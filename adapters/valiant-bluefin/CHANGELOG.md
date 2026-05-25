# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.1] - 2026-04-29

### Changed

- Updated `pg-wire` bound to `>=0.2 && <0.3`, which switches the
  per-connection prepared-statement cache to SIEVE. No public API
  change in this adapter.

## [0.1.0.0] - 2026-04-25

Initial release. Bluefin effect adapter for the
[`valiant`](https://hackage.haskell.org/package/valiant) PostgreSQL
runtime. Provides a `ValiantHandle` parameterised by an effect tag, a
`runValiantB` runner, and explicit-handle versions of the standard
query, command, and transaction operations.

[0.1.0.1]: https://github.com/joshburgess/valiant/releases/tag/valiant-bluefin-v0.1.0.1
[0.1.0.0]: https://github.com/joshburgess/valiant/releases/tag/v0.1.0.0

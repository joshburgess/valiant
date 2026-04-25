# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.0] - 2026-04-25

Initial release. Pipes streaming adapter for
[`valiant`](https://hackage.haskell.org/package/valiant). Exposes
`selectPipe` (cursor-based, requires a transaction) and
`foldPipe` (single-shot extended-protocol) as
`Producer r IO ()` values.

[0.1.0.0]: https://github.com/joshburgess/valiant/releases/tag/v0.1.0.0

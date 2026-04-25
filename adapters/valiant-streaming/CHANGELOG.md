# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.0] - 2026-04-25

Initial release. Streaming adapter for
[`valiant`](https://hackage.haskell.org/package/valiant) using the
[`streaming`](https://hackage.haskell.org/package/streaming) library.
Exposes `selectStream` (cursor-based) and `foldStream`
(single-shot extended-protocol) as `Stream (Of r) IO ()` values.

[0.1.0.0]: https://github.com/joshburgess/valiant/releases/tag/v0.1.0.0

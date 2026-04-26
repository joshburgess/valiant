# Security policy

Valiant is a database driver. It handles credentials, transports them
over the network, and decodes data from a server you may not fully
trust. Vulnerabilities here have outsized impact, so I take reports
seriously.

## Supported versions

The project is at 0.1.0.0. Until 1.0, only the latest released version
on Hackage receives security patches. Older 0.x versions will not be
backported.

| Version | Supported |
| ------- | --------- |
| 0.1.x   | yes       |
| < 0.1   | no        |

## Reporting a vulnerability

Please **do not** file a public GitHub issue for security reports.

Email `joshualoganburgess@gmail.com` with:

- A description of the issue and its impact.
- Steps to reproduce, ideally with a minimal test case.
- The affected package(s) and version(s).
- Any relevant Postgres server version, OS, and TLS configuration.

If you would like to encrypt the report, ask in the email and I will
provide a public key out of band.

### What to expect

- Acknowledgement within 72 hours.
- An initial assessment within 7 days.
- A coordinated disclosure timeline of up to 90 days from the initial
  report. Faster fixes are preferred when reasonable; slower is
  possible for issues requiring upstream changes (Postgres, TLS
  libraries) or coordinated multi-party fixes.
- Credit in the changelog and release notes, unless you prefer to
  remain anonymous.

## Scope

In scope:

- Authentication bypass or downgrade (SCRAM-SHA-256, MD5, cleartext,
  TLS).
- Wire-protocol parsing flaws that allow a malicious server (or a
  network attacker) to crash the client, leak memory, or execute
  arbitrary code.
- Connection-pool issues that leak credentials or sessions across
  unrelated callers.
- SQL-injection vectors in any first-party API that accepts string
  input intended to be parameterised.
- Cache-poisoning or path-traversal in `valiant prepare` or the GHC
  source plugin.

Out of scope:

- Misuse of `simpleQuery` or other documented "you are constructing the
  SQL yourself" APIs. SQL injection in user-constructed queries is the
  caller's responsibility.
- Vulnerabilities in PostgreSQL itself. Report those to the Postgres
  project.
- Vulnerabilities in transitive Haskell dependencies. I will pass those
  upstream and update bounds, but the original report should go to the
  affected package.
- Denial-of-service via legitimate but expensive queries.

## Cryptography

Valiant uses [`crypton`](https://hackage.haskell.org/package/crypton)
for SCRAM-SHA-256 (PBKDF2 + HMAC-SHA-256) and MD5 hashing, and the
[`tls`](https://hackage.haskell.org/package/tls) package (with
`crypton-x509-store` and `crypton-x509-system` for certificate
handling) for TLS. The library does not implement its own primitives.
If you believe the protocol implementation misuses these libraries,
that is in scope.

## Hardening checklist (for users)

- Always use TLS in production. `pg-wire` supports TLS 1.2 and 1.3.
- Prefer SCRAM-SHA-256 over MD5 or cleartext authentication.
- Use parameterised queries (`Statement` + `?` placeholders) rather
  than string-interpolated SQL. The CLI's `valiant prepare` and the
  GHC plugin enforce this for `.sql` files.
- Pin dependency versions in production via `cabal.project.freeze`.
- Audit your Postgres `pg_hba.conf` to require encrypted authentication
  on all client connections.

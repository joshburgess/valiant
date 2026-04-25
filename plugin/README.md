# valiant-plugin

GHC source plugin for valiant. Validates raw `.sql` files against a
`.valiant/` cache at compile time.

## How it fits

The valiant workflow has three steps:

1. Run [`valiant prepare`](https://hackage.haskell.org/package/valiant-cli)
   against a live PostgreSQL database. The CLI reads each `.sql` file
   in your project, sends it to Postgres via `PREPARE` / `DESCRIBE`,
   and writes the parameter and result types into `.valiant/`.
2. Add `valiant-plugin` to your project's `ghc-options` (typically via
   `-fplugin=Valiant.Plugin`). The plugin watches for `queryFile`
   calls in your Haskell source.
3. At compile time, the plugin reads the cached metadata for each
   referenced `.sql` file and rewrites the call to a typed `Statement`
   value. Mismatches between the SQL types and the Haskell types you
   ask for become compile errors.

No Template Haskell. No code generation. Just metadata lookup at
compile time.

## Enabling the plugin

In your `.cabal` file:

```cabal
ghc-options: -fplugin=Valiant.Plugin
build-depends:
  , valiant
  , valiant-plugin
```

In your Haskell source:

```haskell
import Valiant

listUsers :: Statement () User
listUsers = queryFile "sql/list-users.sql"
```

The `queryFile` call becomes a typed `Statement () User` after the
plugin runs, with the type checked against `.valiant/list-users.sql.json`.

## Documentation

- [Top-level README](https://github.com/joshburgess/valiant#readme):
  project overview and the full setup walkthrough.
- [Tutorial](https://github.com/joshburgess/valiant/blob/main/docs/TUTORIAL.md):
  worked examples.

## License

BSD-3-Clause. See `LICENSE`.

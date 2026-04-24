# valiant example: REST API

A small REST API demonstrating valiant with [scotty](https://hackage.haskell.org/package/scotty).

## Features demonstrated

- **Connection pooling**: `newPool` / `withResource`
- **Typed queries**: `fetchOne`, `fetchAll`, `fetchScalar`, `execute`
- **Pipelined batch inserts**: `executeBatch` for high-throughput writes
- **Transactions**: `withTransaction` with automatic commit/rollback
- **Nullable columns**: `Maybe Text` for columns that can be NULL
- **RETURNING**: `fetchScalar` to get the inserted row's ID

## Running

```bash
# Start a test Postgres instance
eval $(scripts/pg-setup.sh)

# Run the server
cabal run valiant-example

# In another terminal:
curl -X POST http://localhost:3000/seed
curl http://localhost:3000/users
curl http://localhost:3000/posts/recent?n=5
```

## Endpoints

| Method | Path | Description |
|--------|------|-------------|
| GET | `/users` | List all users |
| GET | `/users/:id` | Get user by ID |
| POST | `/users` | Create user (`{"cuName": "...", "cuEmail": "..."}`) |
| DELETE | `/users/:id` | Delete user |
| GET | `/posts/recent?n=10` | List recent published posts |
| GET | `/posts/:id` | Get post by ID |
| POST | `/posts` | Create post (`{"cpAuthorId": 1, "cpTitle": "...", "cpBody": "..."}`) |
| POST | `/posts/:id/publish` | Publish a draft post |
| POST | `/seed` | Seed sample data |

## Project structure

```
example/
├── src/
│   ├── Main.hs       # Scotty routes, pool setup, JSON serialization
│   └── Queries.hs    # Typed SQL statements (what the plugin generates)
├── sql/               # Raw SQL files (for reference)
│   ├── users/
│   │   ├── find_by_id.sql
│   │   ├── list_all.sql
│   │   ├── insert.sql
│   │   ├── insert_batch.sql
│   │   └── delete.sql
│   └── posts/
│       ├── find_by_id.sql
│       ├── list_recent.sql
│       ├── insert.sql
│       └── publish.sql
└── valiant-example.cabal
```

## Notes

This example uses `mkStatement` directly instead of `queryFile` because
it doesn't require the GHC plugin to compile. In a real project, you would:

1. Write `.sql` files in `sql/`
2. Run `valiant prepare` to validate them against your database
3. Use `queryFile "users/find_by_id.sql"` with the plugin enabled
4. The plugin verifies types at compile time and rewrites to `mkStatement`

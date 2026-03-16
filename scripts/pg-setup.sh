#!/usr/bin/env bash
set -euo pipefail

# pg-setup.sh — Start a Postgres instance for integration tests and benchmarks.
#
# Strategy:
#   1. If DATABASE_URL is already set and reachable, use it.
#   2. Otherwise, start a Docker container.
#
# Exports DATABASE_URL to stdout (source this script or eval its output).

CONTAINER_NAME="hsqlx-test-pg"
PG_PORT="${HSQLX_TEST_PORT:-5433}"
PG_USER="hsqlx_test"
PG_PASS="hsqlx_test"
PG_DB="hsqlx_test"

# ── 1. Check existing DATABASE_URL ──────────────────────────────────────────

if [ -n "${DATABASE_URL:-}" ]; then
  if pg_isready -d "$DATABASE_URL" -t 2 >/dev/null 2>&1; then
    echo "Using existing DATABASE_URL" >&2
    echo "export DATABASE_URL=\"$DATABASE_URL\""
    exit 0
  else
    echo "DATABASE_URL is set but Postgres is not reachable, falling through to Docker..." >&2
  fi
fi

# ── 2. Start Docker Postgres ───────────────────────────────────────────────

if ! command -v docker >/dev/null 2>&1; then
  echo "Error: No DATABASE_URL and Docker is not available." >&2
  echo "Either set DATABASE_URL or start Docker Desktop." >&2
  exit 1
fi

if ! docker info >/dev/null 2>&1; then
  echo "Error: Docker is installed but the daemon is not running." >&2
  echo "Start Docker Desktop, then retry." >&2
  exit 1
fi

# Stop existing container if present
if docker ps -a --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
  echo "Removing existing container ${CONTAINER_NAME}..." >&2
  docker rm -f "$CONTAINER_NAME" >/dev/null 2>&1 || true
fi

echo "Starting Postgres in Docker (port ${PG_PORT})..." >&2
docker run -d \
  --name "$CONTAINER_NAME" \
  -e POSTGRES_USER="$PG_USER" \
  -e POSTGRES_PASSWORD="$PG_PASS" \
  -e POSTGRES_DB="$PG_DB" \
  -p "${PG_PORT}:5432" \
  postgres:16-alpine \
  >/dev/null

# Wait for Postgres to be ready
echo -n "Waiting for Postgres..." >&2
for i in $(seq 1 30); do
  if docker exec "$CONTAINER_NAME" pg_isready -U "$PG_USER" >/dev/null 2>&1; then
    echo " ready." >&2
    break
  fi
  if [ "$i" -eq 30 ]; then
    echo " timed out!" >&2
    exit 1
  fi
  echo -n "." >&2
  sleep 1
done

DATABASE_URL="postgres://${PG_USER}:${PG_PASS}@localhost:${PG_PORT}/${PG_DB}"

echo "export DATABASE_URL=\"${DATABASE_URL}\""
echo "export HSQLX_TEST_CONTAINER=\"${CONTAINER_NAME}\""

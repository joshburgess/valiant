#!/usr/bin/env bash
set -euo pipefail

# pg-teardown.sh — Stop and remove the Docker Postgres test container.

CONTAINER_NAME="${HSQLX_TEST_CONTAINER:-hsqlx-test-pg}"

if docker ps -a --format '{{.Names}}' 2>/dev/null | grep -q "^${CONTAINER_NAME}$"; then
  echo "Stopping and removing ${CONTAINER_NAME}..." >&2
  docker rm -f "$CONTAINER_NAME" >/dev/null 2>&1
  echo "Done." >&2
else
  echo "No container ${CONTAINER_NAME} found." >&2
fi

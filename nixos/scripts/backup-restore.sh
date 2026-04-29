#!/usr/bin/env bash
set -euo pipefail

BACKUP_DIR="/var/backups"
PG_BACKUP_DIR="$BACKUP_DIR/postgresql"
VOL_BACKUP_DIR="$BACKUP_DIR/volumes"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

info()    { echo -e "${GREEN}[INFO]${NC} $1"; }
warn()    { echo -e "${YELLOW}[WARN]${NC} $1"; }
error()   { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }

usage() {
  echo "Usage: $0 <command> [args]"
  echo ""
  echo "Commands:"
  echo "  list                              List all available backups"
  echo "  list-db                           List database backups"
  echo "  list-volumes                      List volume backups"
  echo "  restore-db <db-name> <backup>     Restore a database from backup"
  echo "  restore-volume <vol> <backup>     Restore a Docker volume from backup"
  echo ""
  echo "Examples:"
  echo "  $0 list"
  echo "  $0 restore-db example_api example_api-2025-04-28_030000.sql.gz"
  echo "  $0 restore-volume grafana-data grafana-data-2025-04-28_040000.tar.gz"
  exit 1
}

list_backups() {
  echo "=== Database Backups ==="
  if [[ -d "$PG_BACKUP_DIR" ]]; then
    ls -lh "$PG_BACKUP_DIR"/*.sql.gz 2>/dev/null || echo "  (none)"
  else
    echo "  (no backup directory)"
  fi
  echo ""
  echo "=== Volume Backups ==="
  if [[ -d "$VOL_BACKUP_DIR" ]]; then
    ls -lh "$VOL_BACKUP_DIR"/*.tar.gz 2>/dev/null || echo "  (none)"
  else
    echo "  (no backup directory)"
  fi
}

restore_db() {
  local db_name="$1"
  local backup_file="$PG_BACKUP_DIR/$2"

  [[ ! -f "$backup_file" ]] && error "Backup file not found: $backup_file"

  warn "This will DROP and RECREATE database: $db_name"
  read -rp "Are you sure? (y/N) " confirm
  [[ "$confirm" != "y" && "$confirm" != "Y" ]] && { echo "Aborted."; exit 0; }

  info "Dropping database $db_name..."
  sudo -u postgres psql -c "DROP DATABASE IF EXISTS $db_name;"

  info "Creating database $db_name..."
  sudo -u postgres psql -c "CREATE DATABASE $db_name;"

  info "Restoring from $backup_file..."
  gunzip -c "$backup_file" | sudo -u postgres psql -d "$db_name"

  info "Restore complete"
}

restore_volume() {
  local vol_name="$1"
  local backup_file="$VOL_BACKUP_DIR/$2"

  [[ ! -f "$backup_file" ]] && error "Backup file not found: $backup_file"

  warn "This will OVERWRITE the contents of Docker volume: $vol_name"
  read -rp "Are you sure? (y/N) " confirm
  [[ "$confirm" != "y" && "$confirm" != "Y" ]] && { echo "Aborted."; exit 0; }

  info "Restoring volume $vol_name from $backup_file..."
  docker run --rm \
    -v "$vol_name":/target \
    -v "$VOL_BACKUP_DIR":/backup:ro \
    alpine sh -c "rm -rf /target/* && tar xzf /backup/$2 -C /target"

  info "Volume restore complete"
}

[[ $# -lt 1 ]] && usage

case "$1" in
  list)           list_backups ;;
  list-db)        ls -lh "$PG_BACKUP_DIR"/*.sql.gz 2>/dev/null || echo "(none)" ;;
  list-volumes)   ls -lh "$VOL_BACKUP_DIR"/*.tar.gz 2>/dev/null || echo "(none)" ;;
  restore-db)     [[ $# -ne 3 ]] && usage; restore_db "$2" "$3" ;;
  restore-volume) [[ $# -ne 3 ]] && usage; restore_volume "$2" "$3" ;;
  *)              usage ;;
esac

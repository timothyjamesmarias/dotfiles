#!/usr/bin/env bash
set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

info()    { echo -e "${GREEN}[INFO]${NC} $1"; }
warn()    { echo -e "${YELLOW}[WARN]${NC} $1"; }
error()   { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }

usage() {
  echo "Usage: $0 <app-name> <db-user> <db-name> [password]"
  echo ""
  echo "Creates a PostgreSQL database and user for a new app."
  echo "If password is omitted, a random one is generated."
  echo ""
  echo "Examples:"
  echo "  $0 my-app my_app my_app"
  echo "  $0 my-app my_app my_app 'supersecretpassword'"
  exit 1
}

[[ $# -lt 3 ]] && usage

APP_NAME="$1"
DB_USER="$2"
DB_NAME="$3"
DB_PASS="${4:-$(openssl rand -base64 32)}"

info "Creating database: $DB_NAME"
sudo -u postgres psql -c "CREATE DATABASE $DB_NAME;" 2>/dev/null || warn "Database $DB_NAME already exists"

info "Creating user: $DB_USER"
sudo -u postgres psql -c "CREATE USER $DB_USER WITH PASSWORD '$DB_PASS';" 2>/dev/null || warn "User $DB_USER already exists"

info "Granting ownership"
sudo -u postgres psql -c "ALTER DATABASE $DB_NAME OWNER TO $DB_USER;"
sudo -u postgres psql -c "REVOKE ALL ON DATABASE $DB_NAME FROM PUBLIC;"

echo ""
info "Database created successfully"
echo ""
echo "  Database: $DB_NAME"
echo "  User:     $DB_USER"
echo "  Password: $DB_PASS"
echo ""
warn "Save the password — it won't be shown again."
echo ""
echo "Add this line to modules/postgresql.nix (appDatabases list):"
echo ""
echo "    { name = \"$APP_NAME\"; user = \"$DB_USER\"; dbName = \"$DB_NAME\"; }"
echo ""
echo "Then run: sudo nixos-rebuild switch --flake /path/to/nixos#server"

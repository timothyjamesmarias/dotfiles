#!/usr/bin/env bash
set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

info()    { echo -e "${GREEN}[INFO]${NC} $1"; }
warn()    { echo -e "${YELLOW}[WARN]${NC} $1"; }
error()   { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }

NIXOS_DIR="$(cd "$(dirname "$0")/.." && pwd)"

usage() {
  echo "Usage: $0 <app-name> <new-image-sha>"
  echo ""
  echo "Updates the image SHA for an app and rebuilds the system."
  echo ""
  echo "Examples:"
  echo "  $0 example-api sha256:abc123..."
  echo "  $0 example-api \$(docker inspect --format='{{index .RepoDigests 0}}' ghcr.io/you/example-api:latest | cut -d@ -f2)"
  exit 1
}

[[ $# -ne 2 ]] && usage

APP_NAME="$1"
NEW_SHA="$2"
APP_FILE="$NIXOS_DIR/apps/$APP_NAME.nix"

[[ ! -f "$APP_FILE" ]] && error "App file not found: $APP_FILE"

# Validate SHA format
[[ ! "$NEW_SHA" =~ ^sha256:[a-f0-9]{64}$ ]] && error "Invalid SHA format. Expected: sha256:<64 hex chars>"

info "Updating $APP_NAME to $NEW_SHA"

# Replace the imageSha value in the app's nix file
sed -i "s|imageSha = \"sha256:[a-f0-9]*\"|imageSha = \"$NEW_SHA\"|" "$APP_FILE"

info "Updated $APP_FILE"

info "Rebuilding NixOS configuration..."
sudo nixos-rebuild switch --flake "$NIXOS_DIR#server"

info "Deploy complete. Verifying container status..."
docker ps --filter "name=$APP_NAME" --format "table {{.Names}}\t{{.Status}}\t{{.Image}}"

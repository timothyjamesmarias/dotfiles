#!/usr/bin/env bash

set -euo pipefail

info() {
  echo -e "\033[1;34m[INFO]\033[0m $1"
}
error() {
  echo -e "\033[1;31m[ERROR]\033[0m $1"
}

info "🔧 Ensuring prerequisite tools are available..."

check_or_fail() {
  if ! command -v "$1" &>/dev/null; then
    error "$1 not found. Please install it first."
    exit 1
  else
    info "$1 already installed."
  fi
}

# Check core tools
check_or_fail node
check_or_fail npm
check_or_fail cargo
check_or_fail gem
check_or_fail go

info "🚀 Installing language servers..."

npm_install() {
  local packages="$1"
  if npm list -g --depth=0 $packages &>/dev/null; then
    info "$packages already installed."
  else
    info "Installing $packages via npm..."
    npm install -g $packages || error "Failed to install $packages"
  fi
}

gem_install() {
  local gem_name="$1"
  if gem list -i "$gem_name" &>/dev/null; then
    info "$gem_name already installed."
  else
    info "Installing $gem_name via gem..."
    gem install "$gem_name" --user-install || error "Failed to install $gem_name"
  fi
}

go_install() {
  local path="$1"
  local name="$(basename "$path")"
  if command -v "$name" &>/dev/null; then
    info "$name already installed."
  else
    info "Installing $name via go..."
    go install "$path@latest" || error "Failed to install $name"
  fi
}

cargo_install() {
  local name="$1"
  if command -v "$name" &>/dev/null; then
    info "$name already installed."
  else
    info "Installing $name via cargo..."
    cargo install "$name" || error "Failed to install $name"
  fi
}

system_check_or_warn() {
  if ! command -v "$1" &>/dev/null; then
    error "$1 not found. Please install via your system package manager (dnf, brew, etc)."
  else
    info "$1 already installed."
  fi
}

# NPM-based servers
npm_install "typescript typescript-language-server"
npm_install "vscode-langservers-extracted"
npm_install "sql-language-server"
npm_install "intelephense"

# GEM-based
gem_install "ruby-lsp"

# Cargo (none needed right now)

# Go
go_install "golang.org/x/tools/gopls"

# Stylua
if ! command -v stylua &> /dev/null; then
    cargo install stylua || error "Failed to install stylua"
else
    info "stylua already installed."
fi

# System packages
system_check_or_warn "lua-language-server"
system_check_or_warn "rust-analyzer"
system_check_or_warn "clangd"

info "✅ All language servers processed!"

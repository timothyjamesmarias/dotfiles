#!/bin/bash
# Lem Editor Installation Script for macOS
# Date: 2026-03-13
#
# This script installs the Lem editor with webview support on macOS.
# Lem will be installed in ~/opt/lem, with configuration in ~/dotfiles/lem

set -e

echo "=== Lem Editor Installation for macOS ==="
echo "This script will install Lem with webview support."
echo "Installation location: ~/opt/lem"
echo "Configuration location: ~/dotfiles/lem"
echo

# Check if running on macOS
if [[ "$OSTYPE" != "darwin"* ]]; then
    echo "Error: This script is designed for macOS only."
    exit 1
fi

# Check for Homebrew
if ! command -v brew &> /dev/null; then
    echo "Error: Homebrew is not installed. Please install it first:"
    echo "  /bin/bash -c \"\$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
    exit 1
fi

# Install dependencies
echo "=== Installing Dependencies ==="

# Install SBCL if not present
if ! command -v sbcl &> /dev/null; then
    echo "Installing SBCL..."
    brew install sbcl
else
    echo "SBCL is already installed: $(sbcl --version)"
fi

# Install GTK4
echo "Installing GTK4 and dependencies..."
brew install gtk4

# Install qlot if not present
if ! command -v qlot &> /dev/null; then
    echo "Installing qlot..."
    curl -L https://qlot.tech/installer | bash
    echo 'export PATH="$HOME/.qlot/bin:$PATH"' >> ~/.zshrc
    export PATH="$HOME/.qlot/bin:$PATH"
else
    echo "qlot is already installed"
fi

# Set installation directories
LEM_INSTALL_DIR="$HOME/opt/lem"
LEM_CONFIG_DIR="$HOME/dotfiles/lem"

# Clone Lem repository
if [ -d "$LEM_INSTALL_DIR" ]; then
    echo "Lem directory already exists at $LEM_INSTALL_DIR"
    read -p "Do you want to update it? (y/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        cd "$LEM_INSTALL_DIR"
        git pull
    fi
else
    echo "Creating installation directory..."
    mkdir -p "$(dirname "$LEM_INSTALL_DIR")"
    echo "Cloning Lem repository to $LEM_INSTALL_DIR..."
    git clone https://github.com/lem-project/lem.git "$LEM_INSTALL_DIR"
fi

cd "$LEM_INSTALL_DIR"

# Clean any existing build artifacts
echo "Cleaning build artifacts..."
rm -rf .qlot 2>/dev/null || true

# Build Lem with webview support
echo "=== Building Lem with WebView Support ==="
echo "This may take a while on first build due to dependency downloads..."

# Retry logic for potential network issues
MAX_RETRIES=3
RETRY_COUNT=0

while [ $RETRY_COUNT -lt $MAX_RETRIES ]; do
    if make webview; then
        echo "Build successful!"
        break
    else
        RETRY_COUNT=$((RETRY_COUNT+1))
        if [ $RETRY_COUNT -lt $MAX_RETRIES ]; then
            echo "Build failed. Retrying (attempt $((RETRY_COUNT+1))/$MAX_RETRIES)..."
            # Clear cache in case of download issues
            rm -rf .qlot 2>/dev/null || true
            rm -rf ~/.qlot/var/tmp/local-time* 2>/dev/null || true
            sleep 5
        else
            echo "Build failed after $MAX_RETRIES attempts."
            echo "Common issues:"
            echo "  1. Network problems downloading packages"
            echo "  2. Missing WebKitGTK (not fully supported on macOS)"
            echo "Try running: rm -rf .qlot && make webview"
            exit 1
        fi
    fi
done

# Check if build was successful
if [ -f "$LEM_INSTALL_DIR/lem" ]; then
    echo "=== Lem Successfully Built ==="
    echo "Binary location: $LEM_INSTALL_DIR/lem"

    # Create symlink in PATH
    read -p "Do you want to create a symlink in /usr/local/bin? (y/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        sudo ln -sf "$LEM_INSTALL_DIR/lem" /usr/local/bin/lem
        echo "Symlink created. You can now run 'lem' from anywhere."
    fi

    # Set up configuration
    echo
    echo "=== Setting Up Configuration ==="

    # Create config directory if it doesn't exist
    if [ ! -d "$LEM_CONFIG_DIR" ]; then
        mkdir -p "$LEM_CONFIG_DIR"
        echo "Created configuration directory: $LEM_CONFIG_DIR"
    fi

    # Create default init.lisp if it doesn't exist
    if [ ! -f "$LEM_CONFIG_DIR/init.lisp" ]; then
        cat > "$LEM_CONFIG_DIR/init.lisp" << 'EOF'
;;; Lem Configuration File
;;; This file is loaded when Lem starts.

(in-package :lem-user)

;;; Enable line numbers
(line-numbers-mode t)

;;; Use spaces instead of tabs
(setf (variable-value 'indent-tabs-mode :global) nil)
(setf (variable-value 'tab-width :global) 4)

;;; Add your customizations here
EOF
        echo "Created default configuration: $LEM_CONFIG_DIR/init.lisp"
    fi

    # Create symlinks for Lem config
    echo "Creating configuration symlinks..."
    mkdir -p ~/.config/lem
    ln -sf "$LEM_CONFIG_DIR/init.lisp" ~/.config/lem/init.lisp 2>/dev/null || true
    echo "Symlinked: ~/.config/lem/init.lisp -> $LEM_CONFIG_DIR/init.lisp"

    echo
    echo "=== Installation Complete ==="
    echo "Lem installed at: $LEM_INSTALL_DIR"
    echo "Configuration at: $LEM_CONFIG_DIR"
    echo
    echo "To start Lem:"
    echo "  lem                           # If symlink was created"
    echo "  $LEM_INSTALL_DIR/lem         # Direct path"
    echo
    echo "To customize Lem:"
    echo "  Edit: $LEM_CONFIG_DIR/init.lisp"
    echo
    echo "Note: WebView support on macOS may have limitations."
    echo "Alternative frontends:"
    echo "  make ncurses  # Terminal version (more stable)"
    echo "  make sdl2     # Native GUI version"
else
    echo "Error: Build appeared to succeed but lem binary not found."
    exit 1
fi
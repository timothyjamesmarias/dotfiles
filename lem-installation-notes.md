# Lem WebView Installation on macOS

## Date: 2026-03-13
## System: macOS Darwin 25.1.0

## Overview
Installing Lem editor with WebView client on macOS. The WebView client provides a graphical interface with graphics rendering capabilities.

## Prerequisites
According to the official documentation, we need:
- SBCL (Steel Bank Common Lisp)
- libwebkitgtk-6.0-dev (WebKit for GTK)
- qlot (Common Lisp package manager)
- git

## Installation Process

### Step 1: Check existing dependencies

SBCL: ✅ Already installed via Homebrew (version 2.5.10)
qlot: ❌ Not installed
GTK4: Available in Homebrew
WebKitGTK: ⚠️ Not directly available in Homebrew (webkitgtk-6.0-dev is a Linux package)

**macOS Challenge #1:** WebKitGTK is primarily a Linux library. On macOS, we need to find an alternative approach or check if the Lem WebView can work with macOS native WebKit.

### Step 2: Installation Progress

1. GTK4 installed successfully via Homebrew with all dependencies
2. qlot installed successfully via curl installer (version 1.8.3) to ~/.qlot
3. Lem repository cloned successfully

### Step 3: Building Lem WebView

Running `make webview` in the Lem directory. This process:
- Installs Quicklisp to .qlot directory
- Downloads and installs 119 dependencies
- Note: Initial build takes significant time due to dependency downloads

**macOS Issue #1: qlot package download error**
```
Unexpected error: The archive file "local-time-20250622-git.tgz" for "local-time" is the wrong size: expected 563,387, got 131,072
```

This appears to be an incomplete download issue.

**Solution:** Clear the qlot cache and retry:
```bash
rm -rf .qlot
rm -rf ~/.qlot/var/tmp/local-time*
make webview
```

### Step 4: Build Success

After clearing cache, the build completed successfully:
- Binary created at: `~/dotfiles/lem/lem` (125MB)
- All 119 dependencies installed
- Build time: ~10 minutes on first run

### Step 5: Testing

The Lem binary runs successfully. However, the webview frontend may have limitations on macOS due to WebKitGTK not being natively supported.

## Summary

### Successful Installation Steps on macOS
1. Install SBCL via Homebrew
2. Install GTK4 via Homebrew
3. Install qlot via curl installer
4. Clone Lem repository
5. Run `make webview`
6. If download errors occur, clear cache and retry

### macOS-Specific Issues Encountered
1. **WebKitGTK availability** - Not directly available in Homebrew
2. **Package download issues** - Resolved by clearing cache
3. **WebView frontend limitations** - May not fully work due to missing WebKitGTK

### Recommendations for macOS Users
- Consider using the ncurses (terminal) frontend: `make ncurses`
- The SDL2 (GUI) frontend may be more stable: `make sdl2`
- WebView support is experimental on macOS

## Installation Script

Created `install-lem.sh` with:
- Automatic dependency installation
- Retry logic for network issues
- Cache clearing on failure
- Interactive symlink creation
- Proper directory structure:
  - Application installed to: `~/opt/lem/`
  - Configuration stored in: `~/dotfiles/lem/`
  - Auto-symlink created: `~/.config/lem/init.lisp` → `~/dotfiles/lem/init.lisp`

## Directory Structure

Final installation layout:
```
~/opt/lem/                 # Lem application directory
├── lem                    # Main executable (118MB)
├── src/                   # Source code
└── ...                    # Other Lem files

~/dotfiles/lem/            # Configuration directory
├── README.md              # Configuration documentation
└── init.lisp              # Main configuration file

~/.config/lem/             # Lem runtime directory
├── init.lisp -> ~/dotfiles/lem/init.lisp  # Symlink to config
├── history/               # Command history
├── packages/              # Package cache
└── debug.log              # Debug output
```
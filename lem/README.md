# Lem Editor Configuration

This directory contains configuration files for the [Lem editor](https://github.com/lem-project/lem).

## Structure

- `init.lisp` - Main configuration file loaded at startup
- Additional config files can be added here and loaded from `init.lisp`

## Installation

To use this configuration:

1. Install Lem using the provided script:
   ```bash
   bash ~/dotfiles/install-lem.sh
   ```

2. The install script will automatically:
   - Install Lem to `~/opt/lem/`
   - Create a symlink from `~/.config/lem/init.lisp` to this directory
   - Optionally create a `/usr/local/bin/lem` symlink

## Configuration

The `init.lisp` file includes:
- Line numbers enabled by default
- Spaces instead of tabs (4 spaces)
- Hooks for language-specific settings
- Framework for custom functions

## Customization

Edit `init.lisp` to add your personal preferences:
- Key bindings
- Theme settings
- Language-specific configurations
- Custom commands and functions

## Directory Locations

- **Lem Binary**: `~/opt/lem/lem`
- **Config Files**: `~/dotfiles/lem/` (this directory)
- **Config Symlink**: `~/.config/lem/init.lisp`
- **History/Cache**: `~/.config/lem/`

## Notes

- Lem supports multiple frontends: webview, ncurses, SDL2
- On macOS, the ncurses (terminal) frontend is most stable
- WebView support requires GTK4 but has limitations on macOS
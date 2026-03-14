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
## macOS Application Bundle

A native macOS application bundle (`Lem.app`) is automatically created during installation.

### Launching as a macOS App

- **Spotlight**: Press `Cmd+Space`, type "Lem"
- **Finder**: Navigate to `~/Applications` and double-click `Lem.app`
- **Dock**: Drag `Lem.app` to your Dock for quick access

### Recreating the .app Bundle

If you need to recreate the application bundle:

```bash
bash ~/dotfiles/create-lem-app.sh
```

This is useful if:
- You moved the Lem installation
- The .app bundle was accidentally deleted
- You want to update the launcher script

### Updating the App Icon

The Lem.app uses the official Lem logo. To regenerate the icon:

```bash
bash ~/dotfiles/create-lem-icon.sh
```

This script converts `lem.svg` to macOS `.icns` format and updates the app bundle automatically

### How It Works

The `Lem.app` bundle is a lightweight wrapper that:
1. Sets up the correct environment (PATH)
2. Launches the Lem binary from `~/opt/lem/lem`
3. Passes any command-line arguments to Lem

The actual application code remains at `~/opt/lem/`, so updating Lem doesn't require recreating the .app bundle.

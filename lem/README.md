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
- Vi mode enabled by default
- Project sessionizer (`M-p` keybinding)
- Framework for custom functions

## Perspectives (Project Sessions)

Lem uses a **perspective-based** system for managing multiple projects, similar to Emacs perspective-mode or tmux sessions. Each perspective is an isolated workspace with its own buffer list, frame, and project context.

### Quick Start

**Launch sessionizer**: `M-p` (Alt/Meta + p)
- Fuzzy-search your projects
- Select a project to create/switch to its perspective

### Key Concepts

**Perspective** = Isolated workspace for a project
- Has its own buffer list (files you open only appear in that perspective)
- Associated with a visual frame (window layout)
- Tied to a project root directory
- Completely separate from other perspectives

### Comparison to Tmux

| Tmux | Lem Perspectives |
|------|------------------|
| tmux sessions | Perspectives |
| tmux windows | Buffers (files, terminals) |
| tmux panes | Lem window splits |
| `C-f 1/2/3` switch windows | `C-x b` switch buffers (within perspective) |
| Session persists | Perspective exists until killed |

### Usage

**Sessionizer** (recommended):
- `M-p` → Fuzzy-find and switch to project
- Automatically creates perspective for new projects
- Reuses existing perspective if project already open

**Manual commands**:
- `M-x perspective-new-command` - Create new perspective manually
- `M-x perspective-kill-command` - Close current perspective and all its buffers
- `M-x perspective-rename-command` - Rename current perspective
- `M-x perspective-list-command` - Show all perspectives with buffer counts

### How It Works

1. **Select project** (`M-p`) → Scan directories, fuzzy-search
2. **First visit** → Creates perspective + frame, opens README if exists
3. **Subsequent visits** → Switches to existing perspective with all your buffers intact
4. **Buffer isolation** → Buffers only visible in their perspective

When you open a file, it belongs to the current perspective. When you switch perspectives, you see a completely different set of buffers.

### Configuration

**Search paths** (edit `sessionizer.lisp`):
```lisp
(setf lem-sessionizer:*sessionizer-search-paths*
      (list (merge-pathnames "code/" (user-homedir-pathname))
            (merge-pathnames "work/" (user-homedir-pathname))))

(setf lem-sessionizer:*sessionizer-max-depth* 3)
```

**Project detection** (Projectile-compatible markers):
- Version control: `.git`, `.hg`, `_FOSSIL_`, `.bzr`, `_darcs`
- Project files: `.projectile`, `.project`, `Makefile`, `TAGS`
- Language-specific: `package.json`, `Cargo.toml`, `Gemfile`, `pom.xml`, etc.

### Terminal Integration

Terminals (`M-x terminal`) are just buffers, so they belong to the current perspective. This means:
- Server logs stay in the project's perspective
- Each project can have its own terminals
- Switch projects → terminals don't clutter your buffer list

### Frame Management

Each perspective gets one frame:
- `C-z n/p` - Navigate between perspective frames
- `C-z r` - Rename frame (also renames perspective)
- `C-z d` - Delete frame (not recommended - use `M-x perspective-kill-command` instead)

### Mental Model

Think of perspectives as **project workspaces**:
- You're always "in" a perspective
- Everything you do (open files, run terminals) happens in that perspective
- Switching perspectives = switching your entire working context
- Like tmux sessions, but inside Lem

### Future Features (Not Yet Implemented)

- Persistence across Lem restarts
- Process management per perspective
- Session templates (like tmux profiles)
- Auto-restore last perspective on startup

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

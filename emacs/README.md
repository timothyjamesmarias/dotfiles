# Emacs Configuration

Vanilla Emacs configuration built from scratch, migrated from tmux + neovim workflow.

## Installation

### 1. Install Emacs

```bash
brew install emacs-plus@30 --with-native-comp
```

Or if using the existing Brewfile:

```bash
brew bundle --file=~/dotfiles/Brewfile
```

### 2. Symlink Configuration

From the dotfiles directory:

```bash
cd ~/dotfiles
stow emacs zsh  # Include zsh for emacsclient aliases
```

This will symlink `~/.config/emacs/` to `~/dotfiles/emacs/.config/emacs/`.

### 3. Start Emacs Daemon (Auto-starts on login)

The daemon is configured to start automatically at login via LaunchAgent.

To start it manually now:

```bash
launchctl load ~/Library/LaunchAgents/gnu.emacs.daemon.plist
```

To stop it:

```bash
launchctl unload ~/Library/LaunchAgents/gnu.emacs.daemon.plist
```

### 4. First Launch

Connect to the daemon:

```bash
e  # Opens new Emacs frame (GUI)
```

Or open in terminal:

```bash
et  # Opens in terminal
```

On first launch:
- Straight.el will bootstrap and install packages (takes 2-5 minutes)
- Wait for all packages to install
- You may see some warnings - this is normal on first run

### 5. Install Nerd Icons Fonts

Required for icons in doom-modeline, dirvish, etc:

```
M-x nerd-icons-install-fonts
```

### 6. Install vterm Dependencies

vterm requires compilation:

```bash
# On macOS
brew install cmake libtool

# The first time you open vterm, it will compile automatically
# Just open Emacs and run: M-c (or SPC o t)
```

## Usage

### Opening Emacs

With the daemon running (auto-starts on login), use these aliases:

```bash
e          # Open new GUI frame
et         # Open in terminal
ec         # Open new GUI frame (same as e)
emacs      # Alias to emacsclient (opens in GUI)
```

All of these connect to the running daemon for instant startup!

### Killing the Daemon

If you need to restart the daemon (after config changes):

```bash
# Kill the daemon
emacsclient -e "(kill-emacs)"

# Restart it
launchctl start gnu.emacs.daemon
```

Or reload your shell (the daemon will restart automatically on next login).

## Configuration Structure

```
~/.config/emacs/
├── early-init.el          # Performance & package bootstrap
├── init.el                # Module orchestrator
├── custom.el              # Auto-generated custom settings
└── modules/
    ├── core.el            # Basic Emacs settings
    ├── evil.el            # Vim keybindings
    ├── completion.el      # Vertico/Consult/Embark
    ├── projects.el        # Projectile
    ├── vterm.el           # Terminal integration
    ├── git.el             # Magit
    ├── navigation.el      # Code navigation (dumb-jump, etags)
    ├── docs.el            # Org-mode & Markdown
    ├── keybindings.el     # Global keybindings (tmux M-* migration)
    └── ui.el              # Theme, which-key, visual settings
```

## Key Bindings

### Tmux M-* Bindings (Preserved)

| Key   | Action                 |
|-------|------------------------|
| M-o   | Find file (project)    |
| M-d   | Find file              |
| M-r   | Ripgrep (project)      |
| M-y   | File manager (dired)   |
| M-n   | New file               |
| M-c   | Toggle vterm           |
| M-C   | Claude Code vterm      |
| M-g   | Magit status           |
| M-G   | Magit dispatch         |
| M-S   | Switch project         |
| M-1-9 | Switch to tab 1-9      |
| M-Tab | Recent tab             |

### Leader Key (SPC)

| Key   | Action                 |
|-------|------------------------|
| SPC f | Files                  |
| SPC b | Buffers                |
| SPC p | Projects (Projectile)  |
| SPC g | Git (Magit)            |
| SPC s | Search                 |
| SPC c | Code navigation        |
| SPC o | Open (terminals, etc.) |
| SPC n | Notes (org-mode)       |
| SPC h | Help                   |
| SPC t | Toggle                 |
| SPC w | Windows                |
| SPC q | Quit                   |

## Workflow Migration

### From tmux-sessionizer to Projectile

- **Before**: `M-S` → tmux-sessionizer (FZF project switch)
- **After**: `M-S` → projectile-switch-project (same UX!)

### From tmux panes to vterm

- **Before**: Multiple tmux panes per session
- **After**: Multiple vterm buffers per project
  - `M-c` → Toggle vterm (project-specific)
  - `SPC o t` → Open project terminal
  - `SPC o T` → New terminal
  - `SPC o c` → Claude Code terminal

### From git.zsh to Magit

Your 600+ line git.zsh maps almost 1:1 to Magit:

- `glog` → `SPC g l l` (magit-log)
- `ga` → `SPC g s` (magit-stage)
- `gu` → `SPC g u` (magit-unstage)
- `gcmsg` → `SPC g C c` (magit-commit)
- `gfix` → `SPC g C f` (magit-commit-fixup)
- `gsq` → `SPC g r i` (magit-rebase-interactive)

Or just: `M-g` → Full Magit interface with action menus!

### From FZF to Vertico/Consult

All your FZF workflows work the same:

- File finding: Same fuzzy matching
- Ripgrep: Live search with previews
- Buffer switching: Fuzzy completion
- Action menus: Embark (like your git action menus)

## Tips

### Multiple Claude Code Projects

You mentioned working with 3-5 project sessions with Claude instances:

1. Use `M-S` to switch between projects (like tmux sessions)
2. Each project gets its own vterm buffers
3. Open Claude in each project: `SPC o c`
4. Switch between vterm buffers: `SPC o V`

### Tabs for Multiple Projects

Work on multiple projects simultaneously:

- `M-1` through `M-9` → Jump to specific tab
- Each tab = separate project workspace
- Like tmux windows, but project-aware

### Generate Tags for Jump-to-Definition

For better code navigation (especially Java/Kotlin):

```
SPC c t g   # Generate ctags for project
gd          # Jump to definition (dumb-jump or ctags)
gb          # Jump back
```

### Discover Commands

Press `SPC` and wait 0.3s → which-key shows all available commands!

## Troubleshooting

### Packages not installing

```
M-x straight-pull-all
M-x straight-rebuild-all
```

### vterm compilation fails

Make sure you have cmake and libtool:

```bash
brew install cmake libtool
```

Then restart Emacs.

### Performance issues

Check GC stats: `M-x emacs-uptime`

If slow, increase GC threshold in `early-init.el`.

## Philosophy

This config follows your dotfiles philosophy:

- **Modular**: Each module is self-contained (like your zsh config)
- **No abstraction layers**: Vanilla Emacs, no Doom/Spacemacs
- **Version-controlled**: Everything in git
- **Understandable**: Clear, commented configuration

Built to preserve your muscle memory from:
- IdeaVim (937 lines of keybindings!)
- tmux workflow (M-* quick launchers)
- git.zsh (600+ lines of git workflows)
- FZF-driven everything

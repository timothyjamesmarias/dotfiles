# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal dotfiles repository managed using **GNU Stow**. Each top-level directory represents a separate package that can be symlinked into the home directory using stow.

## Managing Dotfiles with Stow

### Installing/Symlinking Configurations
```bash
# Install a specific package (creates symlinks in ~/)
stow <package-name>

# Examples:
stow nvim      # Links nvim/.config/nvim -> ~/.config/nvim
stow zsh       # Links zsh/.zshrc -> ~/.zshrc
stow tmux      # Links tmux/.tmux.conf -> ~/.tmux.conf
stow alacritty # Links alacritty/.alacritty.toml -> ~/.alacritty.toml
```

### Unlinking Configurations
```bash
# Remove symlinks for a package
stow -D <package-name>
```

### Restowing (useful after changes)
```bash
# Re-link (useful after adding/modifying files)
stow -R <package-name>
```

## Repository Structure

The repository follows stow's expected structure where each directory represents a separate configuration package:

- `nvim/` - Neovim configuration (Lua-based with lazy.nvim)
- `zsh/` - Zsh shell configuration with extensive aliases and functions
- `tmux/` - Tmux configuration with custom keybindings
- `alacritty/` - Alacritty terminal emulator config
- `git/` - Git configuration including delta pager setup
- `lazygit/` - Lazygit TUI configuration and themes
- `scripts/` - Custom utility scripts
- `ideavim/` - IntelliJ IDEA vim emulation config
- `ctags/` - Ctags configuration
- `intelephense/` - PHP language server config

## Neovim Configuration Architecture

### Entry Point
- `nvim/.config/nvim/init.lua` - Requires the main `tim` module
- `nvim/.config/nvim/lua/tim/init.lua` - Loads core modules in order:
  1. `tim.set` - Editor settings
  2. `tim.remap` - Keybindings
  3. `tim.commands` - Custom commands
  4. `tim.lazy_init` - Plugin manager initialization

### Plugin Management
Uses **lazy.nvim** as the plugin manager. Each plugin is configured in its own file under `nvim/.config/nvim/lua/tim/lazy/`:

- `lsp.lua` - LSP configuration with Mason for automatic language server installation
  - Installed LSPs: lua_ls, ruby_lsp, ts_ls, html, cssls, volar, kotlin_language_server, jdtls, sqlls, rust_analyzer
  - Uses nvim-cmp for completion
- `telescope.lua` - Fuzzy finder with extensive keybindings
- `formatter.lua` - Code formatting (stylua for Lua, rustfmt for Rust)
- `treesitter.lua` - Syntax highlighting
- `gitsigns.lua` - Git integration
- `tmuxnav.lua` - Seamless tmux/nvim navigation
- Other plugins: autopairs, autotag, gutentags, rails, surround, etc.

### Key Neovim Keybindings (Leader = Space)
- `<leader>fs` - Telescope find files
- `<leader>fw` - Telescope live grep
- `<leader>fb` - Telescope buffers
- `<leader>fm` - Format current file
- `<leader>q` - Close buffer without closing window
- `<C-n>/<C-p>` - Next/previous buffer
- Dynamic buffer switching: `<leader>b1` through `<leader>b9` auto-map to first 9 buffers

## Zsh Configuration Features

The zsh configuration (`zsh/.zshrc`) includes:

### Vi Mode
- Uses vi keybindings with visual indicators `[I]` (insert) and `[N]` (normal)
- Custom `jj` mapping for escape in insert mode

### Key Aliases & Tools
- `vim` → `nvim`
- `lg` → `lazygit`
- `ts` → `tmux-sessionizer` (fuzzy project switcher)
- `ff` - Fuzzy find files and open in nvim
- `cdd` - Fuzzy find directories and cd
- `rgnvim` - Ripgrep with fuzzy selection, opens in nvim at line

### Git Workflow Functions
- `gcb` - Fuzzy checkout git branch
- `ga` - Fuzzy stage files
- `gu` - Fuzzy unstage files
- `gcmsg` - Fuzzy commit with selected files
- `ghviewf` - Open file in GitHub web UI

### Development Aliases
- Rust: `rr` (run), `rc` (check), `rt` (test), `rl` (clippy), `rb` (build)
- PHP Laravel: `art` (php artisan), `sail` (Laravel Sail)
- Kubernetes: `k` (kubectl)

### Path Setup
Includes paths for: asdf, cargo, local scripts, composer, homebrew, go

## Tmux Configuration

### Prefix Key
- Changed from `C-b` to `C-f`

### Key Tmux Keybindings (using Alt/Meta)
- `M-s` - tmux-sessionizer (fuzzy project switcher)
- `M-o` - Open file with fuzzy finder
- `M-d` - Change directory with fuzzy finder
- `M-g` - Open lazygit
- `M-r` - Ripgrep fuzzy search
- `M-y` - Open yazi file manager
- `M-p` - Create GitHub PR (gh pr create)
- `M-n` - Create new file with fuzzy directory selection

### Tmux Behavior
- Prefix: `C-f`
- Split horizontal: `C-f h`
- Split vertical: `C-f v`
- Vi copy mode enabled
- Seamless vim-tmux navigation via christoomey/vim-tmux-navigator
- Status bar at top, centered window indicators

## Custom Scripts

Located in `scripts/.local/scripts/`:

- `tmux-sessionizer` - Fuzzy find projects in `~` and `~/projects`, create/switch tmux sessions
- `slugify` - Convert filenames to URL-friendly slugs
- `cheatsheet` - Personal cheatsheet viewer
- `lsp-install.sh` - LSP installation helper
- `launch` / `env-launch.sh` - Project launching utilities

## Git Configuration

Uses **delta** as the pager with:
- Line numbers enabled
- Diff3 conflict style
- Navigate mode enabled
- Base16 syntax theme

## Development Workflow

### Starting a New Session
1. Use `tmux-sessionizer` (bound to `M-s` in tmux or `ts` alias in shell)
2. Fuzzy select project directory
3. Opens/switches to tmux session named after the project

### Common File Operations
- Find and edit files: `ff` (in shell) or `<leader>fs` (in nvim)
- Search in files: `rgnvim` (in shell) or `<leader>fw` (in nvim)
- Navigate directories: `cdd` (in shell)

### Git Workflow
1. Use `lg` (lazygit) for visual git interface, or
2. Use zsh git functions: `ga` (stage), `gcmsg` (commit), `gp` (push)
3. Create PRs: `gpr` alias or `M-p` in tmux

## SSH Configuration

The zshrc automatically manages ssh-agent and adds the GitHub SSH key (`~/.ssh/github`) on shell startup.

## Platform-Specific Notes

- Runs on macOS (Darwin)
- Clipboard operations use `pbcopy` on macOS, `wl-copy`/`xclip` on Linux
- Tmux copy mode adapts to platform (pbcopy vs wl-copy)

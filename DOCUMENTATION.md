# Comprehensive Dotfiles Documentation

## Project Overview

This is a **GNU Stow-based dotfiles repository** for macOS/Linux that provides a highly modular, terminal-centric development environment. The configuration philosophy emphasizes:

- **Modularity**: Shell configuration split into domain-specific files
- **FZF-first workflows**: Nearly everything uses fuzzy finding for interactive selection
- **Terminal productivity**: Heavy use of tmux, neovim, and shell functions
- **Developer-focused**: Strong support for JVM (Kotlin/Java), TypeScript/JavaScript, Ruby, Docker, and Git workflows
- **Symlink management**: GNU Stow for clean, reversible dotfile installation

---

## Repository Structure

### GNU Stow Organization

The repository uses GNU Stow's structure where each top-level directory represents a "package" that gets symlinked to `$HOME`:

```
dotfiles/
├── alacritty/           → ~/.alacritty.toml
├── ctags/               → ~/.ctags.d/
├── editorconfig/        → ~/.editorconfig
├── git/                 → ~/.gitconfig
├── nvim/                → ~/.config/nvim/
├── ripgrep/             → ~/.ripgreprc
├── scripts/             → ~/.local/scripts/
├── tmux/                → ~/.tmux.conf
└── zsh/                 → ~/.zshrc + ~/.config/zsh/
```

**Installation pattern**: `cd ~/dotfiles && stow zsh` creates symlinks in `$HOME` pointing back to `~/dotfiles/zsh/`

---

## Shell Configuration (Zsh)

### Main Entry Point: `.zshrc`

**Location**: `zsh/.zshrc`

**Purpose**: Orchestrates the entire shell environment by sourcing modular configuration files

**Key responsibilities**:
- Sets up PATH with priority: asdf → local scripts → cargo → homebrew → go
- Configures basic environment (EDITOR=nvim, LANG, HOMEBREW settings)
- Sources modular config files in dependency order
- Integrates external tools (fzf, SDKMAN, Angular CLI)

**Module loading order**:
1. `prompt.zsh` - Visual prompt and vi mode
2. `ssh.zsh` - SSH agent setup
3. `keybindings.zsh` - Keyboard shortcuts
4. `aliases.zsh` - Basic aliases
5. `files.zsh` - File/project search utilities
6. `git.zsh` - Git workflows
7. `utils.zsh` - Utility functions
8. `kotlin.zsh` - JVM/Gradle tooling
9. `docker.zsh` - Container management

### Modular Configuration Files

#### 1. **aliases.zsh** - Basic Command Shortcuts

**Location**: `zsh/.config/zsh/aliases.zsh`

**Core aliases**:
- `vim` → `nvim` (always use Neovim)
- `so` → `source ~/.zshrc` (reload shell config)
- `sot` → `tmux source ~/.tmux.conf` (reload tmux)
- `ts` → `tmux-sessionizer` (fuzzy project switcher)
- `cl` → `claude` (Claude CLI)
- `yy` → `yazi` (terminal file manager)
- `clipdir` - Cross-platform clipboard (pbcopy/wl-copy/xclip detection)

**Docker shortcuts**:
- `dcu` → `docker compose up -d`
- `dcd` → `docker compose down`

**Laravel/Sail**:
- `art` → `php artisan`
- `sail` - Laravel Sail wrapper

#### 2. **git.zsh** - Advanced Git Workflows

**Location**: `zsh/.config/zsh/git.zsh`

**Philosophy**: Git operations are primarily fzf-driven for interactive selection with rich previews

**Key functions**:

**Branch management**:
- `git_checkout` (`gcb`) - Fuzzy select and switch branches (including remotes)
- `gb <name>` - Create new branch

**Staging & committing**:
- `git_stage` (`ga`) - Multi-select files to stage with fzf
- `git_stage_patch` (`gap`) - Interactive staging (`git add -p`) on selected file
- `git_unstage` (`gu`) - Fuzzy unstage files
- `git_commit_fzf` (`gcmsg`) - Stage + commit workflow
- `gclaude` (`gcl`) - AI-assisted commit messages using Claude CLI

**Viewing changes**:
- `git_diff_fzf` (`gdf`) - Select files, preview diffs (handles staged/unstaged)
- `git_diff_staged_fzf` (`gdfs`) - View staged changes only
- `git_log_fzf` (`glog`) - Browse commits with diff previews

**History & reflog**:
- `gfh <file>` - View commit history for a specific file
- `gfhf` - File history with fuzzy file selection
- `grl` - Fuzzy reflog browser (checkout/reset/show actions)

**Advanced workflows**:
- `gfix` - Create fixup commits (for autosquash rebase)
- `grba [base]` - Interactive rebase with autosquash
- `git_reset_soft_fzf` (`grsf`) - Soft reset to selected commit

**Worktrees**:
- `gwta <branch> [dir]` - Create worktree for branch
- `gwtl` - List worktrees
- `gwts` - Fuzzy switch to worktree directory
- `gwtr` - Remove worktree

**GitHub integration**:
- `view_in_github_fzf` (`ghviewf`) - Open file in GitHub with line numbers
- `gpr` → `gh pr create --web`

**Basic aliases**:
- `gs` → `git status -sb`
- `gc` → `git commit -v`
- `gca` → `git commit --amend`
- `gpu` → `git push -u origin HEAD`
- `gpf` → `git push --force-with-lease`

#### 3. **files.zsh** - File & Project Navigation

**Location**: `zsh/.config/zsh/files.zsh`

**Core concepts**: Fuzzy finding with rich previews (using bat when available)

**File search**:
- `ff` - Find file and open in nvim
- `ffa` - Multi-select files to open
- `index` - List all files (excludes node_modules, .git, target)
- `cdd` - Fuzzy directory navigation with tree preview

**Content search (ripgrep integration)**:
- `rgnvim` (`rgf`) - Interactive ripgrep → jump to match in nvim
  - No args: live search as you type
  - With pattern: search then select
- `rgq <pattern>` - Populate nvim quickfix list with search results
- `gjs` - Search JS/TS files
- `gcss` - Search CSS/SCSS files
- `gerb` - Search ERB templates

**File management**:
- `cf` - Create new file with fuzzy directory selection
- `del` - Fuzzy multi-select files to delete (interactive)
- `deld` - Fuzzy delete directories (interactive)
- `deldf` - Force delete directories (no confirmation)
- `re` - Open recent files from `~/.recentfiles`

**Directory utilities**:
- `t` - Tree view (2 levels, excludes common build dirs)

#### 4. **utils.zsh** - Utility Functions

**Location**: `zsh/.config/zsh/utils.zsh`

**AWS**:
- `awsctx [profile]` - Switch AWS profiles (fuzzy select if no arg)

**File renaming**:
- `rf` → `rename-file` - Interactive file renaming
- `rfn` → `rename-file --no-keep-path`

**Image optimization** (ImageMagick-based):
- `imgopt <image> [quality]` - In-place optimization (strips metadata, compresses)
- `imgopt-copy <image> [output] [quality]` - Non-destructive optimization
- `imgopt-batch [quality] [--recursive]` - Batch optimize all images
- `imgconvert <file> <ext> [width]` - Convert formats (especially SVG → PNG)

**Configuration**:
- `export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"` - Global rg settings

#### 5. **kotlin.zsh** - JVM/Gradle Development

**Location**: `zsh/.config/zsh/kotlin.zsh`

**Core concept**: Gradle wrapper detection + fuzzy selection for tests/files

**Gradle wrapper**:
- `gw` - Auto-detect `./gradlew` or `../gradlew` and execute
- `gwb` - Build
- `gwc` - Clean
- `gwcb` - Clean build
- `gwt` - Run tests
- `gwr` - Run application

**Testing**:
- `gwtest [class] [method]` - Run specific test (fuzzy select if no args)
- `gwtest-at <file> [line]` - Run test at cursor position (IDE integration)
- `gwtw` - Continuous testing

**Spring Boot**:
- `gwboot` - Run Spring Boot app
- `gwbootd` - Run with debugger
- `gwbootjar` - Build executable JAR

**Project navigation**:
- `ktsrc` - Jump to Kotlin source directory (fuzzy select)
- `kttest` - Jump to Kotlin test directory
- `ktf` - Find Kotlin files and open in nvim
- `ktclass [query]` - Search for class definitions

**Dependencies**:
- `gwdep [query]` - Search dependencies (fuzzy if no query)
- `gwrefresh` - Refresh dependencies
- `gwcache` - Clean Gradle cache

**Debugging**:
- `gwdebug [port]` - Start with debug agent (default: 5005)
- `gwscan` - Build with scan report

**Utilities**:
- `gwmodules` - List project modules
- `gwmain` - Find and run main class (fuzzy select)
- `kttags` - Generate ctags for Kotlin

#### 7. **docker.zsh** - Container Management

**Location**: `zsh/.config/zsh/docker.zsh`

**Philosophy**: Every operation is fzf-driven with rich inspect previews

**Container operations**:
- `docker_exec_fzf` (`dex`) - Exec into container (tries bash, falls back to sh)
- `docker_logs_fzf` (`dl`) - View/follow logs
- `docker_stop_fzf` (`dst`) - Multi-select containers to stop
- `docker_stop_all` (`dsa`) - Stop all containers (with confirmation)
- `docker_rm_fzf` (`drx`) - Remove containers
- `docker_restart_fzf` (`drs`) - Restart containers
- `docker_inspect_fzf` (`dins`) - View container details (uses jq)

**Images**:
- `docker_image_fzf` (`dim`) - Browse images (actions: remove, inspect, run)

**Networks & Volumes**:
- `docker_network_fzf` (`dnet`) - Inspect networks
- `docker_volume_fzf` (`dvol`) - Manage volumes (inspect, remove)

**Utilities**:
- `docker_port_fzf` (`dport`) - View port mappings
- `docker_stats_fzf` (`dstat`) - Monitor container stats
- `docker_cp_fzf` (`dcp`) - Copy files from containers
- `docker_ps_tree` (`dtree`) - Pretty container listing
- `docker_find <pattern>` - Find containers by name

**Cleanup**:
- `docker_prune_fzf` (`dprune`) - Interactive cleanup menu

**Docker Compose**:
- `docker_compose_fzf` (`dcomp`) - Service management (up/down/restart/logs/exec)
- `dc` → `docker compose`
- `dcl` - Follow logs

#### 8. **prompt.zsh** - Shell Prompt

**Location**: `zsh/.config/zsh/prompt.zsh`

**Features**:
- **Vi mode indicator**: `[I]` (green) for insert, `[N]` (red) for normal
- **Current directory** (yellow)
- **Git branch** (magenta, via vcs_info)

**Prompt format**: `[I] ~/path/to/dir (main) > `

#### 9. **keybindings.zsh** - Keyboard Shortcuts

**Location**: `zsh/.config/zsh/keybindings.zsh`

**Vi mode**: `bindkey -v` (enabled globally)

**Custom bindings**:
- `Ctrl-P` / `Ctrl-N` - History navigation (up/down)
- `Alt-n` - Open nvim
- `Alt-k` - Clear screen
- `Alt-f` - Open cmd-finder (fuzzy command search)

**cmd-finder widget**: Searches all aliases, functions, keymaps (from zsh, vim, tmux, git) and inserts selected command into prompt

#### 10. **ssh.zsh** - SSH Agent Management

**Location**: `zsh/.config/zsh/ssh.zsh`

**Behavior**:
- Auto-starts ssh-agent if not running
- Auto-adds `~/.ssh/github` key if not loaded
- Silent operation (no noise on shell startup)

---

## Scripts Directory

**Location**: `scripts/.local/scripts/`

All scripts are executable and in PATH via `.zshrc`

### 1. **tmux-sessionizer**

**Purpose**: Fuzzy project switcher for tmux

**Workflow**:
1. Find directories in `~` and `~/projects` (max depth 2)
2. Select project with fzf
3. Create tmux session named after directory (or switch if exists)
4. Start session in project directory

**Usage**: `ts` (aliased) or bind to tmux key

### 2. **rename-file**

**Purpose**: Interactive file renaming with git-awareness

**Features**:
- Fuzzy file selection with bat preview
- Auto-prepends directory path (just edit filename)
- Uses `git mv` when file is tracked
- Creates directories if needed
- Validates paths

**Usage**:
```bash
rename-file                    # Interactive mode
rename-file old.txt new.txt    # Direct mode
rename-file --no-keep-path     # Don't prepend path
```

**Aliases**:
- `rf` - Rename file
- `rfn` - Rename without path

### 4. **slugify**

**Purpose**: Rename files to URL-friendly slugs

**Transformation**:
- Transliterates to ASCII
- Converts to lowercase
- Replaces non-alphanumeric with hyphens
- Trims leading/trailing hyphens

**Usage**: `slugify file1.txt "My Document.pdf"`

**Example**: `"My Cool File (2024).pdf"` → `"my-cool-file-2024.pdf"`

### 5. **cmd-finder**

**Purpose**: Fuzzy search through all commands, aliases, functions, and keybindings

**Sources**:
- All zsh aliases from modular files (aliases.zsh, git.zsh, files.zsh, utils.zsh, kotlin.zsh, docker.zsh)
- All zsh functions from those files
- Vim keymaps (from nvim lua config)
- Git config aliases
- Tmux keybindings

**Features**:
- Shows category, command name, and description
- Live preview of implementation
- Ctrl-Y to copy command
- Used via `Alt-f` keybinding

**Output format**:
```
category      command         description
git           gcb            Fuzzy switch to git branch
vim           <leader>ff     Find files
```

### 6. **install-deps**

**Purpose**: Bootstrap entire development environment

**Components**:
- **CLI tools** (via Homebrew): fzf, ripgrep, bat, fd, neovim, tmux, etc.
- **LSP servers** (via npm): TypeScript, Vue, PHP (Intelephense), Tailwind, HTML/CSS/JSON
- **Version managers**: asdf, SDKMAN
- **Fonts**: Nerd Fonts (JetBrains Mono, Fira Code, Hack)
- **Neovim dependencies**: pynvim, neovim npm package

**Usage**:
```bash
install-deps --all          # Full installation
install-deps --cli          # CLI tools only
install-deps --lsp          # LSP servers only
install-deps --fonts        # Nerd Fonts only
install-deps --stow         # Symlink dotfiles
```

**Platform support**: macOS (Homebrew), Linux (Homebrew + apt/dnf)

### 7. **install-lsp**

**Purpose**: Simpler LSP server installer (subset of install-deps)

**Servers**: TypeScript, Vue, Intelephense (PHP), Tailwind CSS, HTML/CSS/JSON

**Usage**:
```bash
install-lsp              # Install all
install-lsp typescript   # Install specific
install-lsp --list       # Show available
```

### 8. **sync-macos-apps**

**Purpose**: Compile AppleScript apps from source

**Workflow**:
1. Find `.applescript` files in `macos-apps/`
2. Compile to `.app` bundles in `~/Applications/`
3. Add bundle identifiers for `duti` compatibility
4. Mark as droplets (accept files)

**Usage**: `sync-macos-apps`

**Use case**: Create custom file associations (e.g., open text files in Neovim via Finder)

### 9. **launch**

**Purpose**: Tmux session launcher for Alacritty

**Behavior**:
- Creates or attaches to "default" tmux session
- Runs `tmux-sessionizer` on startup
- Theme management (not fully implemented)

**Usage**: Called via Alacritty shell args in `.alacritty.toml`

### 10. **env-launch.sh**

**Purpose**: Bootstrap script for Alacritty

**Behavior**: Sleeps briefly, then calls `launch --boot`

**Integration**: Set in Alacritty config as shell program

---

## Tool Configurations

### Git (`.gitconfig`)

**Location**: `git/.gitconfig`

**Key settings**:
- **Editor**: nvim
- **Pager**: delta (better diffs with syntax highlighting)
- **Delta features**: side-by-side, line numbers, hyperlinks
- **Merge tool**: nvimdiff (3-way merge)
- **Conflict style**: diff3 (shows common ancestor)

### Tmux (`.tmux.conf`)

**Location**: `tmux/.tmux.conf`

**Core configuration**:
- **Prefix**: `Ctrl-f` (instead of `Ctrl-b`)
- **Vi mode**: Copy mode uses vi keys
- **Mouse**: Enabled
- **Base index**: 1 (windows and panes)
- **Status bar**: Top, centered
- **Default shell**: zsh with --login

**Plugins**:
- `tmux-plugins/tpm` - Plugin manager
- `tmux-plugins/tmux-sensible` - Sensible defaults
- `christoomey/vim-tmux-navigator` - Seamless vim/tmux navigation

**Keybindings**:
- `r` - Reload config
- `h` - Split horizontal
- `v` - Split vertical
- `H/J/K/L` - Resize panes
- `Alt-o` - New window → fuzzy file search (`ff`)
- `Alt-d` - New window → fuzzy directory (`cdd`)
- `Alt-r` - New window → ripgrep search (`rgf`)
- `Alt-y` - New window → yazi file manager
- `Alt-p` - New window → create GitHub PR
- `Alt-c` - Split → Claude CLI
- `Alt-g` - Split → git commit workflow
- `Alt-s` - New window → tmux-sessionizer

**Copy mode**:
- `v` - Begin selection
- `y` - Copy (pbcopy on macOS, wl-copy on Linux)

**Theme**: Custom minimal theme (gray on dark background)

### Alacritty (`.alacritty.toml`)

**Location**: `alacritty/.alacritty.toml`

**Configuration**:
- **Font size**: 13
- **Theme**: Carbonfox (Nightfox variant)
- **Shell**: zsh with custom launch script
- **Option as Alt**: Both (enables Alt keybindings)
- **Padding**: None

**Launch behavior**: Runs `env-launch.sh` which creates/attaches default tmux session

### Ripgrep (`.ripgreprc`)

**Location**: `ripgrep/.ripgreprc`

**Global excludes**:
- `.git/`
- `node_modules/`
- `wp-content/` (WordPress cache/plugins)
- `public/critical-styles/`

### EditorConfig (`.editorconfig`)

**Location**: `editorconfig/.editorconfig`

**Defaults**:
- Tabs for indentation (size 4)
- LF line endings
- UTF-8 encoding
- Trim trailing whitespace
- Insert final newline

**Overrides**:
- **JS/TS/Vue/JSON/YAML**: 2 spaces
- **Lua**: Tabs (4)
- **Ruby/ERB/Slim**: 2 spaces
- **Markdown**: Don't trim trailing whitespace

### CTags (`default.ctags`)

**Location**: `ctags/.ctags.d/default.ctags`

**Configuration**:
- Recursive tagging
- Tag-relative paths
- Extra fields and tags

**Excludes**: `.git`, `node_modules`, `vendor`, `log`, `tmp`, `dist`, `build`, `coverage`

**Language mappings**:
- HTML includes: `.blade.php`, `.slim`, `.erb`

**Languages**: PHP, Ruby, JavaScript, TypeScript, HTML, CSS, SCSS, Lua, Go, Rust, Python

### Neovim

**Location**: `nvim/.config/nvim/`

**Structure**: Uses lazy.nvim plugin manager

**Integration**: Heavily integrated with shell (via `ff`, `rgf`, `rgq`, etc.)

---

## Key Patterns & Philosophy

### 1. **FZF-First Workflow**

Nearly every operation offers fuzzy selection:
- File operations (`ff`, `cdd`, `cf`, `del`)
- Git operations (`gcb`, `ga`, `glog`, `gdf`)
- Docker operations (every docker function)
- Gradle/Kotlin (`gwtest`, `ktf`, `ktsrc`)
- Project switching (`tmux-sessionizer`)

**Benefits**:
- Reduces cognitive load (don't remember exact names)
- Visual confirmation before actions
- Rich previews (bat, git show, docker inspect)

### 2. **Modular Shell Configuration**

**Pattern**: Each concern has its own file (`git.zsh`, `docker.zsh`, etc.)

**Benefits**:
- Easy to locate functionality
- Can source subsets if needed
- Clear responsibility boundaries
- Easy to extend

**Convention**:
- Aliases at the bottom of each file
- Functions above aliases
- Related utilities grouped together

### 3. **Terminal-Centric Development**

**Core tools**:
- Neovim (not IDE)
- Tmux (not terminal tabs)
- Fuzzy finding (not file trees)
- Ripgrep (not IDE search)

**Philosophy**: Mouse-free, keyboard-driven, composable tools

### 4. **Git-Aware Operations**

Multiple tools respect git state:
- `rename-file` uses `git mv` for tracked files
- `git.zsh` functions understand staging/unstaging
- `.gitconfig` uses delta for better diffs

### 5. **Cross-Platform Compatibility**

**Detection pattern**: `if [[ $(uname) == "Darwin" ]]` or `if [[ "$OSTYPE" == "darwin"* ]]`

**Examples**:
- Clipboard: pbcopy (macOS) vs wl-copy/xclip (Linux)
- Package managers: Homebrew everywhere, plus apt/dnf on Linux
- Sed syntax: `-i ''` on macOS vs `-i` on Linux

### 6. **Developer Experience Focus**

**Conveniences**:
- AI-assisted commits (`gclaude`)
- Fixup commits for clean history (`gfix`)
- Test running from shell (`gwtest`)
- Image optimization (`imgopt`)
- Command discovery (`cmd-finder`)

### 7. **Interactive by Default**

**Pattern**: Most operations prompt for confirmation or selection

**Examples**:
- `docker_stop_all` - Shows containers first, asks to confirm
- `rename-file` - Shows file, lets you edit name
- `git_stage` - Multi-select with preview

### 8. **Backup & Safety**

**Patterns**:
- `--preview-only` flags for dry runs
- Confirmation prompts for destructive ops
- `--force-with-lease` instead of `--force`

### 9. **Rich Previews**

**Tools**:
- `bat` - Syntax-highlighted file previews
- `tree` - Directory structure previews
- `git show` - Commit previews
- `docker inspect` - Container details

**Integration**: Every fzf invocation has a `--preview` option

### 10. **Naming Conventions**

**Functions**:
- Descriptive names: `git_checkout`, `docker_exec_fzf`
- Suffix `_fzf` for interactive versions
- Category prefixes: `git_*`, `docker_*`

**Aliases**:
- Short mnemonics: `gcb` (git checkout branch), `dex` (docker exec)
- Consistent patterns: `g*` for git, `d*` for docker, `gw*` for gradle
- One/two letter for common ops: `ga`, `gc`, `gs`

**Scripts**:
- Kebab-case: `tmux-sessionizer`
- Descriptive: `install-deps`, `sync-macos-apps`

---

## Integration Map

### How Everything Connects

```
Alacritty (terminal)
  └─> env-launch.sh
      └─> launch script
          └─> Tmux (multiplexer)
              ├─> Zsh (shell)
              │   ├─> Modular configs
              │   ├─> FZF integration
              │   ├─> Scripts in PATH
              │   └─> Neovim (via aliases)
              └─> Keybindings (Alt-* shortcuts)
                  ├─> File search (ff)
                  ├─> Git workflows (gcmsg)
                  ├─> Project switching (tmux-sessionizer)
                  └─> Claude integration
```

### Tool Ecosystem

**Search & Navigation**:
- FZF - Fuzzy finder (core of everything)
- Ripgrep - Fast content search
- Fd - Fast file finding
- Bat - File previews
- Tree - Directory structure

**Development**:
- Neovim - Text editor
- LSP servers - Language support
- CTags - Code navigation
- Delta - Git diffs

**Git**:
- Git - Version control
- GitHub CLI (`gh`) - PR creation
- Delta - Better diffs
- Custom functions - Workflow automation

**Containers**:
- Docker - Containerization
- Docker Compose - Multi-container apps
- Custom functions - Interactive management

**JVM**:
- Gradle - Build tool
- SDKMAN - Version management
- Custom functions - Test running, project navigation

**Languages** (via LSP/asdf):
- TypeScript/JavaScript - Web dev
- Kotlin/Java - JVM dev
- Ruby - Rails dev
- Rust - Systems programming
- Go - Backend services

---

## Quick Reference: Most-Used Commands

### File Operations
```bash
ff           # Find file → open in nvim
ffa          # Find multiple files
cdd          # Change directory (fuzzy)
rgf          # Search content → jump to match
cf           # Create file (fuzzy dir selection)
```

### Git Workflows
```bash
gcb          # Checkout branch (fuzzy)
ga           # Stage files (multi-select)
gcmsg        # Stage + commit workflow
gdf          # View diffs (fuzzy select)
glog         # Browse commits
gpr          # Create PR on GitHub
```

### Docker
```bash
dex          # Exec into container
dl           # View logs
dst          # Stop containers
dim          # Manage images
dcomp        # Compose service manager
```

### Gradle/Kotlin
```bash
gwb          # Build
gwt          # Test all
gwtest       # Run specific test
ktf          # Find Kotlin file
```

### Tmux
```bash
ts           # Tmux sessionizer (project switcher)
Alt-s        # Same, from within tmux
Alt-o        # File search in new window
Alt-r        # Ripgrep search
Alt-g        # Git commit workflow
```

### Utilities
```bash
rr           # Refactor rename
rf           # Rename file
cmd-finder   # Search all commands (Alt-f)
install-deps # Bootstrap environment
```

---

## Setup & Installation

### First-Time Setup

1. **Clone repository**:
   ```bash
   git clone <your-repo> ~/dotfiles
   cd ~/dotfiles
   ```

2. **Install dependencies**:
   ```bash
   ./scripts/.local/scripts/install-deps --all
   ```

3. **Symlink dotfiles**:
   ```bash
   stow alacritty ctags editorconfig git nvim ripgrep scripts tmux zsh
   ```
   Or:
   ```bash
   ./scripts/.local/scripts/install-deps --stow
   ```

4. **Restart shell**:
   ```bash
   source ~/.zshrc
   ```

5. **Install Tmux plugins**:
   - Open tmux
   - Press `Prefix + I` (Ctrl-f + I)

6. **Open Neovim** to install plugins:
   ```bash
   nvim
   ```

### Updating

- **Pull changes**: `git pull`
- **Re-stow**: `stow <package>` (overwrites existing symlinks)
- **Update dependencies**: `install-deps --cli` or `install-deps --lsp`

---

## File Locations Quick Reference

**Configuration files** (after stowing):
- `~/.zshrc` → Main shell config
- `~/.config/zsh/` → Modular shell files
- `~/.config/nvim/` → Neovim config
- `~/.tmux.conf` → Tmux config
- `~/.gitconfig` → Git config
- `~/.alacritty.toml` → Terminal config
- `~/.ripgreprc` → Ripgrep config
- `~/.local/scripts/` → Custom scripts

**Key directories**:
- `~/dotfiles/` → This repository
- `~/.tmux/plugins/` → Tmux plugins
- `~/Applications/` → Compiled AppleScript apps

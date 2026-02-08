# Workspace Tools

A composable system for managing tmux sessions with declarative profiles, git worktree integration, and Claude AI agent roles.

## Philosophy

**Composable, not replacing** - These tools work independently or together with your existing workflow. Nothing is overridden - only pure additions.

## Quick Start

### 1. Load Shell Aliases

The workspace aliases are in `zsh/.config/zsh/workspace.zsh`. Source it in your `.zshrc`:

```bash
source ~/.config/zsh/workspace.zsh
```

Or restart your shell to pick up the changes automatically.

### 2. Detect Your Project Type

```bash
cd ~/projects/my-rails-app
wsd  # alias for: ws-manager detect
# Output: rails
```

### 3. Apply a Profile

In a tmux session:

```bash
wsa rails  # alias for: ws-manager apply rails
```

This will create windows and panes according to the Rails profile.

### 4. List Available Profiles

```bash
wsl  # alias for: ws-manager list
```

## Architecture

```
workspace-tools/
├── bin/              # Implementation scripts
│   ├── profile-detect    # Detect project type
│   └── profile-apply     # Apply profile to tmux session
├── lib/              # Shared libraries
│   └── yaml-parser.sh    # YAML parsing functions
└── profiles/         # Workspace templates
    ├── default.yaml
    ├── rails.yaml
    ├── kotlin-gradle.yaml
    └── vue.yaml
```

### Entrypoint

- **`ws-manager`** - Main entrypoint in `scripts/.local/scripts/`
  - Thin wrapper that calls tools in `workspace-tools/bin/`
  - Keeps complex logic encapsulated
  - Only `scripts/` needs to be in PATH

## Components

### 1. Profile System

**Profiles** are YAML files that define:
- Project type detection rules (which files identify this project type)
- Tmux window/pane layout
- Commands to run in each pane
- Claude agent roles (coming in Phase 2)

#### Profile Structure

```yaml
name: rails
description: Ruby on Rails application workspace

# Files that identify this project type
detect:
  - Gemfile
  - config/application.rb

# Tmux layout definition
tmux:
  windows:
    - name: editor
      panes:
        - {cmd: "nvim"}

    - name: server
      panes:
        - {cmd: "bin/rails server"}

    - name: console
      layout: main-vertical
      panes:
        - {cmd: "bin/rails console"}
        - {cmd: "tail -f log/development.log"}

# Claude agent roles (Phase 2)
claude:
  roles:
    - name: rails-reviewer
      description: Rails code reviewer
      prompt: |
        You are a Ruby on Rails code reviewer...
```

### 2. Profile Detection

**`profile-detect`** analyzes a directory and returns the matching profile name.

```bash
# Detect project type in current directory
profile-detect
# Output: rails

# Detect in specific directory
profile-detect ~/projects/my-kotlin-app
# Output: kotlin-gradle

# Get full path to profile file
profile-detect --path
# Output: /Users/you/dotfiles/workspace-tools/profiles/rails.yaml

# List all available profiles
profile-detect --list
```

**How it works:**
1. Reads each profile's `detect` rules
2. Checks if those files exist in the target directory
3. Returns the first matching profile
4. Falls back to `default` if nothing matches

### 3. Profile Application

**`profile-apply`** creates tmux windows and panes according to a profile.

```bash
# Apply rails profile to current session
profile-apply rails

# Apply to specific session
profile-apply kotlin-gradle myproject-session

# Dry run (see what would be created)
profile-apply --dry-run rails

# Verbose output
profile-apply --verbose rails
```

**What it does:**
1. Validates the profile file
2. Creates windows defined in the profile
3. Splits panes within each window
4. Runs specified commands in each pane
5. Applies layouts (main-vertical, tiled, etc.)

### 4. Workspace Manager

**`ws-manager`** is the main entrypoint that wraps the tools.

```bash
# Detect project type
ws-manager detect [directory]

# Apply profile to current session
ws-manager apply <profile>

# List available profiles
ws-manager list
```

## Shell Aliases

Defined in `zsh/.config/zsh/workspace.zsh`:

### Basic Commands

```bash
wsd                  # ws-manager detect
wsa <profile>        # ws-manager apply
wsl                  # ws-manager list
```

### Quick Profile Application

```bash
ws-rails             # Apply rails profile
ws-kotlin            # Apply kotlin-gradle profile
ws-vue               # Apply vue profile
ws-default           # Apply default profile
```

### Utility Functions

```bash
ws-setup             # Interactive: detect and prompt to apply
ws-info [dir]        # Show detected profile and session info
```

## Usage Examples

### Example 1: New Rails Project

```bash
cd ~/projects/my-rails-app

# Detect project type
wsd
# Output: rails

# Start tmux session
tmux new -s my-rails-app

# Apply Rails profile
wsa rails

# Result: Creates windows for editor, server, console, test, shell
```

### Example 2: Existing Session

```bash
# Already in a tmux session
tmux

# Apply profile to current session
wsa kotlin-gradle

# Windows are created in current session
```

### Example 3: Custom Profile

```bash
# Create custom profile
cat > workspace-tools/profiles/my-custom.yaml <<EOF
name: my-custom
description: My custom workspace

tmux:
  windows:
    - name: code
      panes:
        - {cmd: "nvim"}
    - name: serve
      panes:
        - {cmd: "python -m http.server"}
EOF

# Apply it
wsa my-custom
```

### Example 4: Interactive Setup

```bash
cd ~/projects/some-project

ws-setup
# Output:
# Detected project type: vue
# Apply this profile to current tmux session? [Y/n]
# y
# Applying profile: vue
# Creating 4 window(s)...
# Profile applied successfully!
```

## Composability

Each tool works **standalone**:

```bash
# Use profile-detect without ws-manager
$HOME/dotfiles/workspace-tools/bin/profile-detect

# Use profile-apply directly
$HOME/dotfiles/workspace-tools/bin/profile-apply rails

# Use in scripts
PROFILE=$(profile-detect ~/projects/myapp)
if [[ "$PROFILE" == "rails" ]]; then
    echo "This is a Rails project"
fi
```

## Integration with Existing Tools

This system is **additive** - it doesn't replace your existing workflow:

- **tmux-sessionizer** - Still works as-is
- **Git worktree aliases** (`gwta`, `gwtl`, `gwts`, `gwtr`) - Unchanged
- **Claude Code integration** (`cc`, `Alt-c`) - Still available
- **Template system** (`new-file`) - Still functional

**New capabilities:**
- Declarative session layouts
- Project-type aware setup
- Future: Claude agent roles, worktree integration

## Creating Custom Profiles

1. Create a YAML file in `workspace-tools/profiles/`
2. Define detection rules, tmux layout, and commands
3. Use with `ws-manager apply <profile-name>`

**Minimal profile:**

```yaml
name: myprofile
description: My custom profile

detect:
  - some-unique-file.txt

tmux:
  windows:
    - name: main
      panes:
        - {cmd: "echo 'Hello World'"}
```

**Profile with layouts:**

```yaml
name: fullstack
description: Full-stack development

detect:
  - docker-compose.yml

tmux:
  windows:
    - name: editor
      panes:
        - {cmd: "nvim"}

    - name: services
      layout: tiled  # tmux layouts: main-vertical, main-horizontal, tiled, even-vertical, even-horizontal
      panes:
        - {cmd: "docker-compose up backend"}
        - {cmd: "docker-compose up frontend"}
        - {cmd: "docker-compose up db"}

    - name: logs
      layout: main-vertical
      panes:
        - {cmd: "docker-compose logs -f backend"}
        - {cmd: "docker-compose logs -f frontend"}
```

## Roadmap

### Phase 1: Profile System ✅ (Current)
- [x] YAML parser
- [x] Profile detection
- [x] Tmux renderer
- [x] Sample profiles (default, rails, kotlin, vue)
- [x] Shell aliases

### Phase 2: Claude Agent Roles (Next)
- [ ] Role definitions (`.claude/roles/*.md`)
- [ ] Role launcher (`claude-agent`)
- [ ] Context injector (git branch, project type, recent commits)
- [ ] Session persistence (resume conversations per session)

### Phase 3: Git Worktree Integration
- [ ] Worktree tools (`worktree-session`)
- [ ] Auto-session creation for worktrees
- [ ] Unified workspace switcher (projects + worktrees)
- [ ] Session mapper (track session↔worktree relationships)

### Phase 4: Advanced Features
- [ ] Workflow orchestration (`feature-start`, `workspace-new`)
- [ ] Session snapshots (save current layout as profile)
- [ ] FZF previews for all fuzzy finders
- [ ] Tmux keybindings (optional)
- [ ] Profile generator (auto-generate from existing sessions)

## Troubleshooting

### Profile not detected

```bash
# Check which files the profile looks for
cat workspace-tools/profiles/rails.yaml | grep -A 10 "detect:"

# Manually specify profile
wsa rails
```

### Commands not in PATH

The entrypoint `ws-manager` should be in `scripts/.local/scripts/`, which should be in your PATH. Check:

```bash
echo $PATH | grep scripts

# If not in PATH, add to .zshrc:
export PATH="$HOME/dotfiles/scripts/.local/scripts:$PATH"
```

### Tmux layout issues

If panes don't arrange correctly:
- Try different layouts: `main-vertical`, `main-horizontal`, `tiled`
- Layouts apply after all panes are created
- Some layouts work better with certain pane counts

### Profile validation errors

```bash
# Test profile syntax
profile-apply --dry-run myprofile

# Validate YAML
yamllint workspace-tools/profiles/myprofile.yaml  # if you have yamllint
```

## Contributing

To add a new profile:

1. Create `workspace-tools/profiles/<name>.yaml`
2. Define `name`, `description`, `detect` rules
3. Add `tmux.windows` with your desired layout
4. Test with `profile-apply --dry-run <name>`
5. Apply with `wsa <name>`

## License

Part of personal dotfiles. Use freely.

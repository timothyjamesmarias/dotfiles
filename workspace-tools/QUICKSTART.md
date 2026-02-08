# Workspace Tools - Quick Start Guide

## Setup (One-time)

### 1. Source the shell aliases

The workspace aliases should already be set up to load automatically. Verify:

```bash
source ~/.config/zsh/workspace.zsh
```

Or simply restart your shell.

### 2. Verify installation

```bash
# Check if ws-manager is accessible
ws-manager help

# List available profiles
wsl
```

## Basic Usage

### Interactive Setup (Recommended for first-time)

```bash
# In your project directory
cd ~/projects/my-rails-app

# Start or attach to tmux session
tmux new -s my-rails-app
# or
tmux attach -t my-rails-app

# Detect and apply profile interactively
ws-setup
```

This will:
1. Detect your project type
2. Prompt you to apply the profile
3. Create windows and panes automatically

### Manual Profile Application

```bash
# In a tmux session, detect project type
wsd

# Apply a specific profile
wsa rails        # Apply rails profile
wsa kotlin       # Apply kotlin-gradle profile
wsa vue          # Apply vue profile
wsa default      # Apply default profile
```

### Quick Aliases

```bash
ws-rails         # Shortcut: apply rails profile
ws-kotlin        # Shortcut: apply kotlin-gradle profile
ws-vue           # Shortcut: apply vue profile
ws-default       # Shortcut: apply default profile
```

## Testing It Out

### Test with the dotfiles directory

```bash
# Navigate to dotfiles
cd ~/dotfiles

# Start tmux session
tmux new -s test-workspace

# Detect profile (should detect 'default')
wsd

# Apply default profile
wsa default
```

You should see:
- Window 0: "editor" with nvim
- Window 1: "shell" with a shell prompt

### Dry Run (Safe Testing)

Before applying to a real session, test what would happen:

```bash
workspace-tools/bin/profile-apply --dry-run rails
```

This shows what windows and panes would be created without actually creating them.

## Creating Your First Custom Profile

1. Create a new profile file:

```bash
cat > workspace-tools/profiles/myproject.yaml <<'EOF'
name: myproject
description: My custom project workspace

detect:
  - myproject-marker.txt

tmux:
  windows:
    - name: code
      panes:
        - {cmd: "nvim ."}

    - name: server
      panes:
        - {cmd: "npm start"}

    - name: logs
      layout: main-vertical
      panes:
        - {cmd: "tail -f logs/app.log"}
        - {cmd: "tail -f logs/error.log"}
EOF
```

2. Test detection:

```bash
# Create marker file in your project
touch ~/projects/myproject/myproject-marker.txt

# Detect
cd ~/projects/myproject
wsd
# Should output: myproject
```

3. Apply it:

```bash
# In tmux session
wsa myproject
```

## Common Workflows

### Workflow 1: New Project Session

```bash
# Create session and apply profile in one go
cd ~/projects/my-rails-app
tmux new -s rails-app -c ~/projects/my-rails-app

# Inside tmux
ws-rails
```

### Workflow 2: Existing Session

```bash
# Attach to existing session
tmux attach -t my-session

# Apply profile to add windows
wsa rails
```

### Workflow 3: Project Info

```bash
# Check what profile would be detected
ws-info ~/projects/my-rails-app

# Output:
# Directory: /Users/you/projects/my-rails-app
# Detected profile: rails
# Current tmux session: my-session
```

## Troubleshooting

### "Not in a tmux session" error

You must be inside a tmux session to apply profiles:

```bash
# Start tmux first
tmux

# Then apply profile
wsa default
```

### Profile not detected correctly

```bash
# Check what files the profile looks for
cat workspace-tools/profiles/rails.yaml | grep -A 5 "detect:"

# Manually specify profile if detection fails
wsa rails  # Force rails profile
```

### Windows not created as expected

Use verbose mode to see what's happening:

```bash
workspace-tools/bin/profile-apply --verbose rails
```

## Next Steps

- Read the full [README.md](./README.md) for advanced usage
- Explore existing profiles in `workspace-tools/profiles/`
- Create custom profiles for your projects
- Wait for Phase 2 (Claude agent roles) and Phase 3 (git worktree integration)

## Tips

1. **Start simple**: Use the default profile first to understand how it works
2. **Dry run**: Always test with `--dry-run` before applying to important sessions
3. **Incremental**: Apply profiles to test sessions, not your main work session initially
4. **Customize**: Copy an existing profile and modify it for your needs
5. **Detect first**: Run `wsd` to see what profile will be applied

## Getting Help

```bash
ws-manager help              # Main help
profile-detect --help        # Detection help
profile-apply --help         # Application help
```

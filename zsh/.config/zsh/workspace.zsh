#!/usr/bin/env zsh
# Workspace management aliases and functions
# Part of the workspace-tools system

# Main workspace manager aliases
alias wsd='ws-manager detect'     # detect project type
alias wsa='ws-manager apply'      # apply profile
alias wsl='ws-manager list'       # list profiles

# Direct access to tools (for power users)
alias profile-detect='$HOME/dotfiles/workspace-tools/bin/profile-detect'
alias profile-apply='$HOME/dotfiles/workspace-tools/bin/profile-apply'

# Quick profile application for common types
alias ws-rails='ws-manager apply rails'
alias ws-kotlin='ws-manager apply kotlin-gradle'
alias ws-vue='ws-manager apply vue'
alias ws-default='ws-manager apply default'

# Interactive workspace setup function
ws-setup() {
    local detected_profile
    detected_profile=$(ws-manager detect)

    echo "Detected project type: $detected_profile"
    echo ""
    echo "Apply this profile to current tmux session? [Y/n]"
    read -r response

    if [[ "$response" =~ ^[Yy]$ ]] || [[ -z "$response" ]]; then
        ws-manager apply "$detected_profile" --verbose
    else
        echo "Profile not applied. You can apply manually with: ws-manager apply $detected_profile"
    fi
}

# Show current session's likely profile
ws-info() {
    local current_dir="${1:-.}"
    echo "Directory: $current_dir"
    echo "Detected profile: $(ws-manager detect "$current_dir")"

    if [[ -n "${TMUX:-}" ]]; then
        echo "Current tmux session: $(tmux display-message -p '#S')"
    else
        echo "Not in a tmux session"
    fi
}

# Placeholder functions for future features
# These will be implemented in later phases

# Claude agent role launcher (Phase 2)
# Uncomment when claude-agent is implemented
# alias car='claude-agent'
# alias cac='claude-agent context'

# Worktree + session integration (Phase 3)
# Uncomment when worktree-session is implemented
# alias wtsc='worktree-session create'
# alias wtsw='worktree-session switch'
# alias wtsl='worktree-session list'
# alias wtsx='worktree-session cleanup'

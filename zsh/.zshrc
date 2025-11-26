# ~/.zshrc - Main entrypoint for zsh configuration
# This file sources modular configuration files from ~/.config/zsh/
autoload -Uz compinit
compinit

# --- Basic environment setup ---
export LANG=en_US.UTF-8
export EDITOR="nvim"

# --- Shell behavior ---
chmod +x "$HOME/.local/scripts"

# --- Path ---
export PATH="$HOME/.asdf/shims:$HOME/.asdf/bin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.local/scripts:$HOME/.composer/vendor/bin:/opt/homebrew/bin:$HOME/go/bin:$HOME/.config/emacs/bin:$PATH"
export ASDF_DATA_DIR=/Users/$(whoami)/.asdf

# --- Homebrew configuration ---
HOMEBREW_NO_AUTO_UPDATE=1

# --- Source modular configuration files ---
ZSHCONFIG="$HOME/.config/zsh"

source "$ZSHCONFIG/prompt.zsh"      # Prompt and vi mode indicator
source "$ZSHCONFIG/ssh.zsh"         # SSH agent setup
source "$ZSHCONFIG/keybindings.zsh" # Custom keybindings
source "$ZSHCONFIG/aliases.zsh"     # Basic aliases
source "$ZSHCONFIG/files.zsh"       # File and project search utilities
source "$ZSHCONFIG/git.zsh"         # Git aliases and functions
source "$ZSHCONFIG/utils.zsh"       # Utility functions
source "$ZSHCONFIG/ast-grep.zsh"    # Structural code search (ast-grep)
source "$ZSHCONFIG/kotlin.zsh"      # Kotlin/Gradle utilities
source "$ZSHCONFIG/docker.zsh"      # Docker utilities

# --- FZF integration ---
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# --- ENV secrets ---
[ -f "$HOME/.env" ] && source "$HOME/.env"

# --- SDKMAN (must be at end) ---
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"


# Load Angular CLI autocompletion.
source <(ng completion script)


# Load Angular CLI autocompletion.
source <(ng completion script)

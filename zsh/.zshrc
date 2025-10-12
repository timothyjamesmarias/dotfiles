# ~/.zshrc - Main entrypoint for zsh configuration
# This file sources modular configuration files from ~/.config/zsh/

# --- Basic environment setup ---
export LANG=en_US.UTF-8
export EDITOR="nvim"

# --- Shell behavior ---
export NVIM_LISTEN_ADDRESS="/tmp/nvim-$$.sock"
echo "$NVIM_LISTEN_ADDRESS" >> ~/.cache/nvim_socket
chmod +x "$HOME/.local/scripts"

# --- Path ---
export PATH="$HOME/.asdf/shims:$HOME/.asdf/bin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.local/scripts:$HOME/.composer/vendor/bin:/opt/homebrew/bin:$HOME/go/bin:$PATH"
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
source "$ZSHCONFIG/rust.zsh"        # Rust development aliases
source "$ZSHCONFIG/utils.zsh"       # Utility functions

# --- FZF integration ---
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# --- ENV secrets ---
[ -f "$HOME/.env" ] && source "$HOME/.env"

# --- SDKMAN (must be at end) ---
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

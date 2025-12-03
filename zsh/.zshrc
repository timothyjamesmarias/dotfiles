# ~/.zshrc - Main entrypoint for zsh configuration
# This file sources modular configuration files from ~/.config/zsh/

# --- Completion system with caching ---
# Only rebuild completion cache once per day
autoload -Uz compinit
if [[ -n ${ZDOTDIR:-~}/.zcompdump(#qN.mh+24) ]]; then
  compinit
else
  compinit -C
fi

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
source "$ZSHCONFIG/heroku.zsh"      # Heroku utilities

# --- FZF integration ---
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# --- ENV secrets ---
[ -f "$HOME/.env" ] && source "$HOME/.env"

# --- SDKMAN (lazy loaded) ---
# Initialize SDKMAN only when needed
export SDKMAN_DIR="$HOME/.sdkman"
sdkman_init() {
  if [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]]; then
    source "$HOME/.sdkman/bin/sdkman-init.sh"
    unfunction sdkman_init
  fi
}

# Lazy load aliases for common SDKMAN commands
for cmd in gradle java kotlin sdk mvn; do
  alias $cmd="sdkman_init && $cmd"
done

# --- Heroku autocomplete (lazy loaded) ---
heroku() {
  unfunction heroku
  HEROKU_AC_ZSH_SETUP_PATH=/Users/timmarias/Library/Caches/heroku/autocomplete/zsh_setup
  [[ -f "$HEROKU_AC_ZSH_SETUP_PATH" ]] && source "$HEROKU_AC_ZSH_SETUP_PATH"
  heroku "$@"
}

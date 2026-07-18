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

# --- Editor ---
# ctx-editor (in ~/.local/scripts) picks the right editor at invocation time:
# JetBrains IDE terminal -> that IDE; Doom server up -> emacsclient; else -> vi.
export EDITOR="ctx-editor"

# --- Shell behavior ---
chmod +x "$HOME/.local/scripts"

# --- Path ---
# asdf 0.16+ is a Go binary (installed via Homebrew); it provides no bin/ dir of
# its own, so only the shims belong on PATH.
export ASDF_DATA_DIR="$HOME/.asdf"
export PATH="$ASDF_DATA_DIR/shims:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.local/scripts:$HOME/.composer/vendor/bin:/opt/homebrew/bin:$HOME/go/bin:$HOME/.config/emacs/bin:$PATH"

# --- Homebrew configuration ---
HOMEBREW_NO_AUTO_UPDATE=1

# --- Source modular configuration files ---
ZSHCONFIG="$HOME/.config/zsh"

source "$ZSHCONFIG/prompt.zsh"      # Prompt and vi mode indicator
source "$ZSHCONFIG/ssh.zsh"         # SSH agent setup
source "$ZSHCONFIG/keybindings.zsh" # Custom keybindings
source "$ZSHCONFIG/aliases.zsh"     # Basic aliases
source "$ZSHCONFIG/git.zsh"         # Git aliases and functions
source "$ZSHCONFIG/utils.zsh"       # Utility functions
source "$ZSHCONFIG/ripgrep.zsh"    # Ripgrep config and rginit

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

export PATH="$PATH:$HOME/Library/Android/sdk/platform-tools"
export PATH="$HOME/.qlot/bin:$PATH"

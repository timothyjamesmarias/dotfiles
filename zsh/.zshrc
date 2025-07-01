# --- Language and shell options ---
export LANG=en_US.UTF-8
export EDITOR="nvim"
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
export OBSIDIAN_USER="Timothy Marias"

# --- vi mode with status ---
bindkey -v

function zle-jj-escape() {
  # Read the next key without showing it
  local key
  read -t 0.3 -k 1 key
  if [[ $key == 'j' ]]; then
    zle vi-cmd-mode
  else
    LBUFFER+="j${key}"
  fi
}
zle -N zle-jj-escape
bindkey -M viins 'jj' zle-jj-escape

# --- Prompt styles ---
autoload -Uz vcs_info
precmd() { vcs_info }

setopt prompt_subst

# Initialize vi mode indicator
VI_MODE="%{$fg_bold[green]%}[I]%{$reset_color%}"

# Update VI_MODE based on keymap state
function zle-keymap-select {
  case $KEYMAP in
    vicmd) VI_MODE="%{$fg_bold[red]%}[N]%{$reset_color%}" ;;   # Normal mode
    main|viins) VI_MODE="%{$fg_bold[green]%}[I]%{$reset_color%}" ;;  # Insert mode
  esac
  zle reset-prompt
}
zle -N zle-keymap-select

# Set vcs info
precmd() {
  vcs_info
}

# Enable prompt substitution
setopt prompt_subst

# Combined prompt (Vi mode + path + git branch)
PROMPT='${VI_MODE} %F{yellow}%~%F{magenta}${vcs_info_msg_0_}%f %# '

# Optional: avoid showing `[N]` on startup
function zle-line-init {
  VI_MODE="%{$fg_bold[green]%}[I]%{$reset_color%}"
  zle reset-prompt
}
zle -N zle-line-init

# --- GitHub SSH Agent Setup ---
#
# Start ssh-agent if not running
if [ -z "$SSH_AUTH_SOCK" ] || ! pgrep -u "$USER" ssh-agent >/dev/null; then
  eval "$(ssh-agent -s)" >/dev/null
fi

# Always add key if not already added
if ! ssh-add -l 2>/dev/null | grep -q "github"; then
  ssh-add ~/.ssh/github >/dev/null 2>&1
fi

# --- Key aliases ---
alias vim="nvim"
alias so="source ~/.zshrc"
alias sot="tmux source ~/.tmux.conf"
alias art="php artisan"
alias sail='[ -f vendor/bin/sail ] && bash vendor/bin/sail'
alias sart="sail artisan"
alias ts="tmux-sessionizer"
alias cl="clear"
alias k="kubectl"
alias lg="lazygit"
alias xclip="xclip -selection c"
alias ch="cheatsheet"
alias bi="brew install"

# --- File + Project Search Utilities ---
alias index="find . -type f | grep -vE 'node_modules|target|.git'"
alias ff="index | fzf | xargs nvim"
alias ffa="index | fzf -m | xargs nvim"
alias fd="find . -type d | grep -vE 'node_modules|target|.git' | fzf"
alias cdd='cd "$(find . -type d | grep -vE "node_modules|target|.git" | fzf --preview "tree -C -L 2 {}")"'
alias pj='cd "$(fd . ~/projects ~/work ~/repos -mindepth 1 -maxdepth 2 -type d | fzf)"'
alias t="tree -L 2 -I 'node_modules|.git|target|dist|*.lock|*.cache'"
alias gjs="rg --glob '**/*.js' --glob '**/*.ts' --glob '!node_modules'"
alias gcss="rg --glob '**/*.scss' --glob '**/*.css' --glob '!node_modules'"
alias re="cat ~/.recentfiles | fzf | xargs nvim"
alias rgf="rg --no-heading --line-number --color=always . \
  | fzf --ansi \
  | awk -F':' '{printf \"nvim +%s %s\\n\", \$2, \$1}' \
  | sh"
alias nfw="rgf | xargs nvim"
alias yy="yazi"
alias del="find . -type f | fzf -m --preview 'bat --style=numbers --color=always {}' | xargs -o rm -i"
alias deld="find . -type d | fzf -m --preview 'bat --style=numbers --color=always {}' | xargs -o rm -rf -i"
alias deldf="find . -type d | fzf -m --preview 'bat --style=numbers --color=always {}' | xargs -o rm -rf"
alias ls='ls -G'

# unalias the command to get it to work
unalias cf 2>/dev/null
cf() {
  local dir
  dir=$(find . -type d -not -path '*/.git/*' -not -path '*/node_modules/*' -not -path '*/target/*' \
    | fzf --preview "tree -C -L 2 {}") || return

  read "filename?Enter new file name (relative to $dir): "
  [[ -z "$filename" ]] && echo "Cancelled." && return

  local filepath="$dir/$filename"
  mkdir -p "$(dirname "$filepath")"
  touch "$filepath"
  nvim "$filepath"
}

alias cf="cf"

# --- Git aliases and Utilities ---

# Fuzzy switch to git branch
git_checkout() {
  local branch
  branch=$(git branch --all | grep -v HEAD | sed 's/.* //' | fzf) && git checkout "$branch"
}

# Fuzzy stage files
git_stage() {
  git status --short | fzf -m | awk '{print $2}' | xargs git add
}

# Fuzzy reset files
git_unstage() {
  git diff --cached --name-only | fzf -m | xargs git reset HEAD --
}

# Fuzzy commit from selected files
git_commit_fzf() {
  git_stage
  echo "Enter commit message:"
  read msg
  git commit -m "$msg"
}

# Fuzzy view commits
git_log_fzf() {
  git log --oneline --graph --decorate --all | fzf --no-sort --reverse --height=40%
}

alias gs='git status -sb'
alias gc='git commit -v'
alias gca='git commit -v --amend'
alias gl='git log --oneline --graph --decorate'
alias gp='git push'
alias gcl='git clone'
alias gpo='git push origin'
alias gpf='git push --force-with-lease'
alias gco='git checkout'
alias gb='git branch -vv'
alias gd='git diff'
alias gds='git diff --staged'
alias gr='git restore'
alias grs='git restore --staged'
alias gcp='git cherry-pick'
alias gsw='git switch'
alias gpr='gh pr create --web'

alias gcb="git_checkout"
alias ga="git_stage"
alias gu="git_unstage"
alias gcmsg="git_commit_fzf"
alias glog="git_log_fzf"

alias gcf="git log --oneline | fzf | cut -d ' ' -f1 | xargs git checkout"

# --- Clipboard alias (cross-platform) ---
if [[ $(uname) == "Linux" ]]; then
  if [[ $XDG_SESSION_TYPE == "wayland" ]]; then
    alias clipdir="pwd | wl-copy"
  else
    alias clipdir="pwd | xclip"
  fi
else
  alias clipdir="pwd | pbcopy"
fi

# --- Rust dev aliases ---
alias rr="cargo run"
alias rc="cargo check"
alias rf="cargo fmt"
alias rt="cargo test"
alias rl="cargo clippy --all-targets --all-features -- -D warnings"
alias ru="cargo update"
alias rd="cargo doc --open"
alias rtf="cargo tree | fzf"
alias rdeps="cargo metadata --format-version=1 | jq '.packages[] | .name'"
alias rsmod="fd . rs | grep '\.rs$' | fzf | xargs nvim"
alias rb="cargo build"
alias rbe="cargo build --release"
alias rx="cargo expand | bat --paging=always"  # Requires `cargo-expand` and `bat`
alias rlog="RUST_LOG=debug cargo run"           # For debugging with env vars
alias rch="cargo check --all-targets --all-features"
alias rbench="cargo bench"
alias rclean="cargo clean"

# --- Shell Behavior ---
export NVIM_LISTEN_ADDRESS="/tmp/nvim-$$.sock"
echo "$NVIM_LISTEN_ADDRESS" >> ~/.cache/nvim_socket
chmod +x "$HOME/.local/scripts"

# --- Path ---
export PATH="$HOME/.asdf/shims:$HOME/.asdf/bin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.local/scripts:$HOME/.composer/vendor/bin:/opt/homebrew/bin:$HOME/go/bin:$PATH"
export ASDF_DATA_DIR=/Users/$(whoami)/.asdf

# --- Make Homebrew not shit ---
HOMEBREW_NO_AUTO_UPDATE=1

# --- History Navigation ---
bindkey '^P' up-history
bindkey '^N' down-history

# --- Keybindings ---
bindkey -s '^[n' 'nvim\n'
bindkey -s '^[k' 'clear\n'

# --- FZF integration ---
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# --- ENV secrets ---
[ -f "$HOME/.env" ] && source "$HOME/.env"

# --- AWS profile switcher ---
awsctx () {
	profile=${1:-noop}
	if [ "$profile" != "noop" ]; then
		export AWS_PROFILE=$profile
	else
		export AWS_PROFILE="$(aws configure list-profiles | fzf)"
		echo "Switched to profile \"$AWS_PROFILE\"."
	fi
}

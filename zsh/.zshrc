# --- Language and shell options ---
export LANG=en_US.UTF-8
export EDITOR="/usr/bin/nvim"
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
export OBSIDIAN_USER="Timothy Marias"

# --- vi mode with status ---
bindkey -v
zle-keymap-select() {
  RPROMPT="${KEYMAP/vicmd/%F{red}[NORMAL]%f}"
  zle reset-prompt
}
zle-line-init() {
  zle -K viins
  RPROMPT=""
}
zle -N zle-keymap-select
zle -N zle-line-init

# --- GitHub SSH Agent Setup ---
if [ -z "$SSH_AUTH_SOCK" ]; then
  eval "$(ssh-agent -s)" > /dev/null
  ssh-add -q ~/.ssh/github 2>/dev/null
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

# --- File + Project Search Utilities ---
alias index="find . -type f | grep -vE 'node_modules|target|.git'"
alias ff="index | fzf | xargs nvim"
alias ffa="index | fzf -m | xargs nvim"
alias fd="find . -type d | grep -vE 'node_modules|target|.git' | fzf"
alias cdd='cd "$(fd --type d --hidden --exclude .git --exclude node_modules --max-depth 6 ~ | fzf --preview "tree -C -L 2 {}")"'
alias pj='cd "$(fd . ~/projects ~/work ~/repos -mindepth 1 -maxdepth 2 -type d | fzf)"'
alias t="tree -L 2 -I 'node_modules|.git|target|dist|*.lock|*.cache'"
alias grepjs="rg --glob '**/*.js' --glob '**/*.ts' --glob '!node_modules'"
alias grepcss="rg --glob '**/*.scss' --glob '**/*.css' --glob '!node_modules'"
alias re="cat ~/.recentfiles | fzf | xargs nvim"
alias rgf="rg --no-heading --line-number --color=always . | fzf --ansi | awk -F':' '{print $1, $2}'"

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

# --- Shell Behavior ---
export NVIM_LISTEN_ADDRESS="/tmp/nvim-$$.sock"
echo "$NVIM_LISTEN_ADDRESS" >> ~/.cache/nvim_socket
chmod +x "$HOME/.local/scripts"

# --- Path ---
export PATH="$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.local/scripts:$HOME/.composer/vendor/bin:/opt/homebrew/bin:$PATH"
export PATH="$PATH:$HOME/.asdf/bin:$HOME/.asdf/shims:$HOME/go/bin"
export ASDF_DATA_DIR=/Users/tim/.asdf

# --- History Navigation ---
bindkey '^P' up-history
bindkey '^N' down-history

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

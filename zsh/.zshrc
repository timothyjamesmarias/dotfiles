export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"
ZVM_VI_INSERT_ESCAPE_BINDKEY=jj

plugins=(
  git 
  ssh-agent
  rails
  asdf 
)
zstyle :omz:plugins:ssh-agent identities github

source $ZSH/oh-my-zsh.sh

export LANG=en_US.UTF-8

alias vim="nvim"
alias so="source ~/.zshrc"
alias sot="tmux source ~/.tmux.conf"
alias art="php artisan"
alias sail='[ -f vendor/bin/sail ] && bash vendor/bin/sail'
alias sart="sail artisan"
alias ts="tmux-sessionizer"
alias cl="clear"
alias k="kubectl"
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

if [[ $(uname) == "Linux" ]]; then
  if [[ $xdg_session_type == "wayland" ]]; then
    alias clipdir="pwd | wl-copy"
  else
    alias clipdir="pwd | xclip"
  fi
else
  alias clipdir="pwd | pbcopy"
fi

alias lg="lazygit"
alias xclip="xclip -selection c"
alias ch="cheatsheet"
export EDITOR="/usr/bin/nvim"
# # assign a listening socket for nvim for each terminal, using the terminal pid
export NVIM_LISTEN_ADDRESS="/tmp/nvim-$$.sock"
echo "$NVIM_LISTEN_ADDRESS" >> ~/.cache/nvim_socket
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
export OBSIDIAN_USER="Timothy Marias"
eval "$(fzf --zsh)"
source "$HOME/.env"
export ASDF_DATA_DIR=/Users/tim/.asdf

export PATH="$PATH:$HOME/go/bin"
export PATH="$PATH:$HOME/.config/emacs/bin"
export PATH="$PATH:$HOME/.local/scripts"
export PATH="$PATH:$HOME/.composer/vendor/bin"
export PATH="$PATH:/Users/timmarias/.composer/vendor/bin"
export PATH="$PATH:$ASDF_DATA_DIR"
export PATH="$HOME/.asdf/bin:$PATH"
export PATH="$HOME/.asdf/shims:$PATH"
chmod +x "$HOME/.local/scripts"

function zle-keymap-select {
  if [[ $KEYMAP == vicmd ]]; then
    RPROMPT="%F{red}[NORMAL]%f"
  else
    RPROMPT=""
  fi
  zle reset-prompt
}
zle -N zle-keymap-select

# Also handle exiting insert mode
function zle-line-init {
  zle -K viins
  RPROMPT=""
}
zle -N zle-line-init


# Created by `pipx` on 2025-05-19 00:24:08
export PATH="$PATH:$HOME/.local/bin"
# Ensure vi mode starts on new lines
bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history

awsctx () {
	profile=${1:-noop}
	if [ $profile != "noop" ]
	then
		export AWS_PROFILE=$profile
	else
		export AWS_PROFILE="$(aws configure list-profiles | fzf)"
		echo "Switched to profile ""$AWS_PROFILE""."
	fi
}

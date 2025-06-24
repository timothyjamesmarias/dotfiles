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
alias cl="clear"
alias urb="./urbit -w $URBIT_ID -k ~/urbit.key"
# alias rails="bin/rails"
alias sail='[ -f vendor/bin/sail ] && bash vendor/bin/sail'
alias sart="sail artisan"
alias ts="tmux-sessionizer"
alias cl="clear"
alias k="kubectl"

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

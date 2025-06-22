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
alias ts="tmux-sessionizer"

# # Automatically start tmux with a login shell if not already inside tmux
# if command -v tmux &> /dev/null; then
#   if [[ -z "$TMUX" ]]; then
#     tmux new-session -d -s default "exec /bin/zsh --login" || tmux attach-session -t default
#   fi
# fi

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

export PATH="$PATH:$HOME/go/bin"
export PATH="$PATH:$HOME/.config/emacs/bin"
export PATH="$PATH:$HOME/.local/scripts"
export PATH="$PATH:$HOME/.composer/vendor/bin"
chmod +x "$HOME/.local/scripts"

# if [[ $(uname) == "Linux" ]]; then
#   . /opt/asdf-vm/asdf.sh
# fi

# if [[ $(uname) == "Darwin" ]]; then
#   source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
# fi
# bindkey '^I' autosuggest-accept
# bindkey -s ^b "tmux-sessionizer\n"


# Created by `pipx` on 2025-05-19 00:24:08
export PATH="$PATH:$HOME/.local/bin"

# --- Basic aliases ---

alias vim="nvim"
alias so="source ~/.zshrc"
alias sot="tmux source ~/.tmux.conf"
alias art="php artisan"
alias sail='[ -f vendor/bin/sail ] && bash vendor/bin/sail'
alias sart="sail artisan"
alias ts="tmux-sessionizer"
alias cl="claude"
alias k="kubectl"
alias lg="lazygit"
alias xclip="xclip -selection c"
alias ch="cheatsheet"
alias bi="brew install"
alias yy="yazi"
alias ls='ls -G'
alias cj='curl -s -H "Content-Type: application/json"'
alias png="ping google.com"

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

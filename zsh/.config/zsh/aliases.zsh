# --- Basic aliases ---

alias vim="nvim"
alias so="source ~/.zshrc"
alias sot="tmux source ~/.tmux.conf"
alias art="php artisan"
alias sail='[ -f vendor/bin/sail ] && bash vendor/bin/sail'
alias sart="sail artisan"
alias ts="tmux-sessionizer"
alias cl="claude"
alias clr="claude --resume"
alias k="kubectl"
alias xclip="xclip -selection c"
alias bi="brew install"
alias yy="yazi"
alias ls='ls -G'
alias cj='curl -s -H "Content-Type: application/json"'
alias png="ping google.com"
alias dcu="docker compose up -d"
alias dcd="docker compose down"
alias cmf="cmd-finder"
alias cmr="cmd-finder --rebuild"
alias mdev='while true; do npm run dev || { [ $? -eq 130 ] && break; sleep 1; }; done'

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

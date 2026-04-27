# --- Basic aliases ---

alias so="source ~/.zshrc"
alias art="php artisan"
alias sail='[ -f vendor/bin/sail ] && bash vendor/bin/sail'
alias sart="sail artisan"
alias cl="claude"
alias clr="claude --resume"
alias k="kubectl"
alias bi="brew install"
alias yy="yazi"
alias ls='ls -G'
alias cj='curl -s -H "Content-Type: application/json"'
alias png="ping google.com"
alias dcu="docker compose up -d"
alias dcd="docker compose down"
alias mdev='while true; do npm run dev || { [ $? -eq 130 ] && break; sleep 1; }; done'
alias -g ...='../..'

# In vterm, fully reset the grid (including scrollback) on `clear`.
if [[ "$INSIDE_EMACS" == *vterm* ]]; then
  alias clear='printf "\e[H\e[2J\e[3J"'
fi

# --- Emacs aliases ---
alias e="emacsclient --socket-name=doom -c -n"          # Open in GUI (new frame)
alias et="emacsclient --socket-name=doom -nw"           # Open in terminal
alias ec="emacsclient --socket-name=doom -c -n"         # Same as e (create frame)
alias emacs="emacsclient --socket-name=doom -c -n"      # Replace regular emacs command

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

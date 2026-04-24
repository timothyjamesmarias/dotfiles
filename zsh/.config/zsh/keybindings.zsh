# --- Keybindings ---

# Vi mode
bindkey -v

# History Navigation
bindkey '^P' up-history
bindkey '^N' down-history

# Edit command line in $EDITOR
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^e' edit-command-line

# Insert last argument from previous commands (cycle with repeat)
bindkey '\e.' insert-last-word

# Copy earlier word from current/previous line
bindkey '\e,' copy-prev-shell-word

# Inline help - show man page for current command
autoload -Uz run-help
unalias run-help 2>/dev/null
bindkey '\eh' run-help

# Transpose words around cursor
bindkey '\et' transpose-words

# Show ... while completing
expand-or-complete-with-dots() {
  printf '\e[31m...\e[0m'
  zle expand-or-complete
  zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey '^I' expand-or-complete-with-dots

# History-based autosuggestion (predict as you type)
autoload -Uz predict-on predict-off
zle -N predict-on
zle -N predict-off
bindkey '^X^P' predict-on
bindkey '^X^O' predict-off

# Custom keybindings
bindkey -s '^[k' 'clear\n'


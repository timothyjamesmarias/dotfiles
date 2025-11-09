# --- Keybindings ---

# Vi mode
bindkey -v

# History Navigation
bindkey '^P' up-history
bindkey '^N' down-history

# Custom keybindings
bindkey -s '^[n' 'nvim\n'
bindkey -s '^[k' 'clear\n'

# cmd-finder widget - fuzzy search for commands/aliases
cmd-finder-widget() {
  # Open fzf in the terminal (takes over the screen)
  local cmd=$(cmd-finder < /dev/tty)

  if [ -n "$cmd" ]; then
    # Insert the command into the command line buffer
    LBUFFER="${LBUFFER}${cmd}"
  fi

  # Redraw the prompt and command line
  zle reset-prompt
  zle redisplay
}
zle -N cmd-finder-widget
bindkey '^[f' cmd-finder-widget  # Alt+f to open cmd-finder


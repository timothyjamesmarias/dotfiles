# --- Prompt styles and vi mode indicator ---

autoload -Uz vcs_info

# Configure vcs_info for git
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' formats ' (%b)'
zstyle ':vcs_info:*' actionformats ' (%b|%a)'

# Only run vcs_info in git repos for performance
precmd() {
  if git rev-parse --is-inside-work-tree &>/dev/null; then
    vcs_info
  else
    vcs_info_msg_0_=""
  fi
}

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

# Combined prompt (Vi mode + path + git branch)
PROMPT='${VI_MODE} %F{yellow}%~%F{magenta}${vcs_info_msg_0_}%f > '

# Optional: avoid showing `[N]` on startup
function zle-line-init {
  VI_MODE="%{$fg_bold[green]%}[I]%{$reset_color%}"
  zle reset-prompt
}
zle -N zle-line-init

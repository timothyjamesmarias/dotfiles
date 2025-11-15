# --- File + Project Search Utilities ---

alias index="find . -type f | grep -vE 'node_modules|target|.git'"
alias ff="index | fzf | xargs nvim"
alias ffa="index | fzf -m | xargs nvim"
alias cdd='cd "$(find . -type d | grep -vE "node_modules|target|.git" | fzf --preview "tree -C -L 2 {}")"'
alias t="tree -L 2 -I 'node_modules|.git|target|dist|*.lock|*.cache'"
alias re="cat ~/.recentfiles | fzf | xargs nvim"
alias del="find . -type f | fzf -m --preview 'bat --style=numbers --color=always {}' | xargs -o rm -i"
alias deld="find . -type d | fzf -m --preview 'bat --style=numbers --color=always {}' | xargs -o rm -rf -i"
alias deldf="find . -type d | fzf -m --preview 'bat --style=numbers --color=always {}' | xargs -o rm -rf"

# Ripgrep + nvim integration
rgnvim() {
  if [ -z "$1" ]; then
    echo "Usage: rgnvim <search_pattern> [additional rg options]"
    return 1
  fi

  rg --no-heading --line-number --color=always --hidden --glob='!.git' "$@" \
    | fzf --ansi --preview 'echo {}' \
    | awk -F':' '{printf "nvim +%s %s\n", $2, $1}' \
    | sh
}

# Ripgrep to quickfix - populate vim quickfix list with search results
rgq() {
  if [ -z "$1" ]; then
    echo "Usage: rgq <pattern> [rg options]"
    echo "Opens nvim with search results in quickfix list"
    return 1
  fi

  local results=$(rg --vimgrep --no-heading --hidden --glob='!.git' "$@")

  if [ -z "$results" ]; then
    echo "No matches found for: $1"
    return 1
  fi

  # Open nvim with results in quickfix
  nvim -q <(echo "$results")
}

# Interactive search - prompts for pattern
alias rgf='rgnvim'
alias gjs='rgnvim --type js --type ts'
alias gcss='rgnvim --type css --type scss'
alias grb='rgnvim --type ruby'
alias gerb='rgnvim --glob="**/*.erb"'

# Search JS/TS files
fjs() {
  rg --no-heading --line-number --color=always \
    --glob '**/*.js' \
    --glob '**/*.ts' \
    --glob '!node_modules' \
    . \
  | fzf --ansi \
  | awk -F':' '{printf "nvim +%s %s\n", $2, $1}' \
  | sh
}

# Search CSS/SCSS files
fcss() {
  rg --no-heading --line-number --color=always \
    --glob '**/*.css' \
    --glob '**/*.scss' \
    --glob '!node_modules' \
    . \
  | fzf --ansi \
  | awk -F':' '{printf "nvim +%s %s\n", $2, $1}' \
  | sh
}

# Create new file with fuzzy directory selection
cf() {
  local dir
  dir=$(find . -type d -not -path '*/.git/*' -not -path '*/node_modules/*' -not -path '*/target/*' \
    | fzf --preview "tree -C -L 2 {}") || return

  read "filename?Enter new file name (relative to $dir): "
  [[ -z "$filename" ]] && echo "Cancelled." && return

  local filepath="$dir/$filename"
  mkdir -p "$(dirname "$filepath")"
  touch "$filepath"
  nvim "$filepath"
}

# Finder integration
finder() {
  local target
  target=$(fd . --type f --type d --hidden --exclude .git --exclude node_modules | fzf --prompt="Open in Finder > " ) [ -n "target "] && open "$target"
}

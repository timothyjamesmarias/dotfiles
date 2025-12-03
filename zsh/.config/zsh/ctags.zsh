# --- Ctags Utilities ---

# Generate ctags for current directory
ctags-generate() {
  local project_root=$(git rev-parse --show-toplevel 2>/dev/null || pwd)
  echo "Generating tags for $project_root..."

  cd "$project_root" || return 1
  ctags -R .

  local tag_count=$(wc -l < tags | tr -d ' ')
  echo "Generated $tag_count tags in $project_root/tags"
}

# Find tag and jump to definition (command line)
ctag-find() {
  if [ -z "$1" ]; then
    echo "Usage: ctag-find <tag-name>"
    return 1
  fi

  local tag_file=$(find_tags_file)
  if [ -z "$tag_file" ]; then
    echo "No tags file found. Run ctags-generate first."
    return 1
  fi

  local result=$(grep "^$1\t" "$tag_file" | head -1)
  if [ -z "$result" ]; then
    echo "Tag not found: $1"
    return 1
  fi

  local file=$(echo "$result" | awk -F'\t' '{print $2}')
  local line=$(echo "$result" | grep -o '/\^.*\$/' | sed 's/[/^$]//g')

  echo "Found in: $file"
  echo "Line: $line"
  echo ""
  echo "Open with: nvim $file +/$1"
}

# Fuzzy search tags and jump to definition
ctag-fzf() {
  local tag_file=$(find_tags_file)
  if [ -z "$tag_file" ]; then
    echo "No tags file found. Run ctags-generate first."
    return 1
  fi

  local selected=$(cat "$tag_file" | grep -v "^!" | \
    fzf --preview 'echo {} | awk -F"\t" "{print \$2}" | xargs bat --color=always --style=numbers --highlight-line $(echo {} | grep -o "[0-9]*" | head -1)' \
        --preview-window=right:60% \
        --delimiter=$'\t' \
        --with-nth=1 \
        --height=80%)

  if [ -n "$selected" ]; then
    local file=$(echo "$selected" | awk -F'\t' '{print $2}')
    local pattern=$(echo "$selected" | awk -F'\t' '{print $3}' | sed 's/[/^$]//g')

    # Open in nvim and search for the pattern
    nvim "$file" +"/$pattern"
  fi
}

# List all tags in current project
ctag-list() {
  local tag_file=$(find_tags_file)
  if [ -z "$tag_file" ]; then
    echo "No tags file found."
    return 1
  fi

  local type="${1:-all}"

  case "$type" in
    functions|f)
      grep -E '\tf\t' "$tag_file" | awk -F'\t' '{print $1}' | sort | uniq
      ;;
    classes|c)
      grep -E '\tc\t' "$tag_file" | awk -F'\t' '{print $1}' | sort | uniq
      ;;
    methods|m)
      grep -E '\tm\t' "$tag_file" | awk -F'\t' '{print $1}' | sort | uniq
      ;;
    all)
      cat "$tag_file" | grep -v "^!" | awk -F'\t' '{print $1"\t"$4}' | column -t -s$'\t'
      ;;
    *)
      echo "Usage: ctag-list [functions|classes|methods|all]"
      return 1
      ;;
  esac
}

# Stats about tags in current project
ctag-stats() {
  local tag_file=$(find_tags_file)
  if [ -z "$tag_file" ]; then
    echo "No tags file found."
    return 1
  fi

  local total=$(grep -v "^!" "$tag_file" | wc -l | tr -d ' ')
  local functions=$(grep -E '\tf\t' "$tag_file" | wc -l | tr -d ' ')
  local classes=$(grep -E '\tc\t' "$tag_file" | wc -l | tr -d ' ')
  local methods=$(grep -E '\tm\t' "$tag_file" | wc -l | tr -d ' ')
  local variables=$(grep -E '\tv\t' "$tag_file" | wc -l | tr -d ' ')

  echo "Tag Statistics"
  echo "──────────────"
  echo "Total:      $total"
  echo "Functions:  $functions"
  echo "Classes:    $classes"
  echo "Methods:    $methods"
  echo "Variables:  $variables"
  echo ""
  echo "Tag file: $tag_file"
  echo "Size: $(du -h "$tag_file" | awk '{print $1}')"
}

# Clean all cached tags
ctag-clean() {
  local cache_dir="$HOME/.cache/tags"
  if [ ! -d "$cache_dir" ]; then
    echo "No tags cache directory found."
    return 0
  fi

  echo "Tags cache directory: $cache_dir"
  echo "Current size: $(du -sh "$cache_dir" | awk '{print $1}')"
  echo ""
  echo -n "Delete all cached tags? (y/n): "
  read confirm

  if [[ "$confirm" =~ ^[Yy]$ ]]; then
    rm -rf "$cache_dir"/*
    echo "Tags cache cleared."
  else
    echo "Cancelled."
  fi
}

# Helper: Find tags file (check current dir, then parent, then cache)
find_tags_file() {
  if [ -f "./tags" ]; then
    echo "./tags"
    return 0
  fi

  local git_root=$(git rev-parse --show-toplevel 2>/dev/null)
  if [ -n "$git_root" ] && [ -f "$git_root/tags" ]; then
    echo "$git_root/tags"
    return 0
  fi

  # Check gutentags cache
  local project_path=$(pwd | sed 's/\//-/g' | sed 's/^-//')
  local cache_file="$HOME/.cache/tags/${project_path}-tags"
  if [ -f "$cache_file" ]; then
    echo "$cache_file"
    return 0
  fi

  return 1
}

# Aliases
alias ctf='ctag-fzf'
alias ctl='ctag-list'
alias cts='ctag-stats'
alias ctg='ctags-generate'
alias ctc='ctag-clean'

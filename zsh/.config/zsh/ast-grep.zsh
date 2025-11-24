# --- ast-grep utilities ---
# Structural code search and refactoring

# Basic search with language detection
sg() {
  ast-grep "$@"
}

# Interactive search with fzf - find structural patterns
sgi() {
  local lang="${1:-}"
  local pattern="${2:-}"

  if [ -z "$lang" ]; then
    lang=$(echo "kotlin\njava\ntypescript\njavascript\nruby\nrust\ngo\npython\nphp\nc\ncpp" | fzf --prompt="Language > ")
    [ -z "$lang" ] && return
  fi

  if [ -z "$pattern" ]; then
    read "pattern?Pattern: "
    [ -z "$pattern" ] && return
  fi

  ast-grep run -p "$pattern" -l "$lang" --json 2>/dev/null \
    | jq -r '.[] | "\(.file):\(.range.start.line):\(.lines)"' \
    | fzf --ansi --preview 'echo {} | cut -d: -f1 | xargs bat --style=numbers --color=always --highlight-line $(echo {} | cut -d: -f2)' \
    | cut -d: -f1-2 \
    | awk -F: '{printf "nvim +%s %s\n", $2, $1}' \
    | sh
}

# Search and replace interactively
sgr() {
  local lang="$1"
  local pattern="$2"
  local replacement="$3"

  if [ -z "$lang" ] || [ -z "$pattern" ] || [ -z "$replacement" ]; then
    echo "Usage: sgr <lang> <pattern> <replacement>"
    echo "Example: sgr kotlin 'println(\$MSG)' 'logger.info(\$MSG)'"
    return 1
  fi

  ast-grep run -p "$pattern" -r "$replacement" -l "$lang" --interactive
}

# Quick language-specific searches
sgkt() { ast-grep run -p "$1" -l kotlin "${@:2}"; }           # Kotlin
sgjava() { ast-grep run -p "$1" -l java "${@:2}"; }           # Java
sgts() { ast-grep run -p "$1" -l typescript "${@:2}"; }       # TypeScript
sgjs() { ast-grep run -p "$1" -l javascript "${@:2}"; }       # JavaScript
sgrb() { ast-grep run -p "$1" -l ruby "${@:2}"; }             # Ruby
sgrs() { ast-grep run -p "$1" -l rust "${@:2}"; }             # Rust
sggo() { ast-grep run -p "$1" -l go "${@:2}"; }               # Go
sgpy() { ast-grep run -p "$1" -l python "${@:2}"; }           # Python

# Common patterns - Kotlin
sgkt-fun() { ast-grep run -p 'fun $NAME($$$PARAMS): $RET { $$$BODY }' -l kotlin "$@"; }
sgkt-class() { ast-grep run -p 'class $NAME($$$PARAMS) { $$$BODY }' -l kotlin "$@"; }
sgkt-data() { ast-grep run -p 'data class $NAME($$$PARAMS)' -l kotlin "$@"; }
sgkt-suspend() { ast-grep run -p 'suspend fun $NAME($$$PARAMS)' -l kotlin "$@"; }
sgkt-flow() { ast-grep run -p 'Flow<$TYPE>' -l kotlin "$@"; }

# Common patterns - Java
sgjava-class() { ast-grep run -p 'public class $NAME { $$$BODY }' -l java "$@"; }
sgjava-method() { ast-grep run -p 'public $RET $NAME($$$PARAMS) { $$$BODY }' -l java "$@"; }
sgjava-interface() { ast-grep run -p 'public interface $NAME { $$$BODY }' -l java "$@"; }

# Find all functions/methods in a file
sg-fns() {
  local lang="$1"
  local path="${2:-.}"

  case "$lang" in
    kotlin|kt)
      ast-grep run -p 'fun $NAME($$$)' -l kotlin "$path"
      ;;
    java)
      ast-grep run -p '$RET $NAME($$$PARAMS) { $$$BODY }' -l java "$path"
      ;;
    typescript|ts)
      ast-grep run -p 'function $NAME($$$)' -l typescript "$path"
      ast-grep run -p 'const $NAME = ($$$) =>' -l typescript "$path"
      ;;
    *)
      echo "Usage: sg-fns <kotlin|java|typescript> [path]"
      ;;
  esac
}

# Output results as JSON for piping
sgj() {
  ast-grep run "$@" --json
}

# Count matches
sgc() {
  ast-grep run "$@" --json 2>/dev/null | jq -s 'length'
}

# --- Kotlin Multiplatform Project Navigation (Rust CLI wrapper) ---
# Shell integration for the kt Rust CLI tool with built-in fzf support

# Path to the kt binary
KT_BIN="${KT_BIN:-$HOME/bin/kt}"

# Check if kt binary exists
if [[ ! -x "$KT_BIN" ]]; then
  if command -v kt &> /dev/null; then
    KT_BIN="kt"
  else
    echo "Warning: kt binary not found. Install from /Users/timmarias/projects/kt" >&2
    return 1
  fi
fi

# --- Main kt command (pass-through) ---
kt() {
  "$KT_BIN" "$@"
}

# --- Interactive wrapper functions ---
# These use the --interactive flag for a better terminal experience

# Interactive file navigation
kti() {
  local cmd="$1"
  shift

  case "$cmd" in
    find|f)
      if [[ -z "$1" ]]; then
        echo "Usage: kti find <pattern>"
        return 1
      fi
      "$KT_BIN" find "$@" --interactive
      ;;
    here|h)
      "$KT_BIN" here --interactive
      ;;
    ctx|ctx-find|cf)
      "$KT_BIN" ctx-find --interactive
      ;;
    all|a)
      "$KT_BIN" all --interactive
      ;;
    ss|sourceset)
      "$KT_BIN" sourceset "$@" --interactive
      ;;
    mod|modules|m)
      "$KT_BIN" modules --interactive
      ;;
    *)
      echo "Unknown command: $cmd"
      echo "Usage: kti {find|here|ctx|all|sourceset|modules}"
      echo ""
      echo "Examples:"
      echo "  kti find Recipe    # Find and open Recipe files with fzf"
      echo "  kti here           # Browse current module files with fzf"
      echo "  kti ctx            # Context-aware browse with fzf"
      echo "  kti ss             # Select source set, then browse"
      echo "  kti mod            # Browse modules and show info"
      return 1
      ;;
  esac
}

# Interactive module navigation with cd
ktg() {
  local module="$1"

  local path
  if [[ -z "$module" ]]; then
    # Interactive module selection (kt goto with no args uses fzf)
    path=$("$KT_BIN" goto 2>&1)
  else
    path=$("$KT_BIN" goto "$module" 2>&1)
  fi

  if [[ $? -eq 0 && -n "$path" ]] && [[ -d "$path" ]]; then
    cd "$path" || return 1
  elif [[ -n "$path" ]]; then
    echo "$path" >&2
    return 1
  fi
}

# --- Convenient aliases ---
alias ktctx='kt context'
alias ktinfo='kt info'
alias ktmod='kt modules'
alias ktcache='kt cache'

# Interactive aliases (use the fzf versions by default)
alias ktf='kti find'
alias kth='kti here'
alias ktc='kti ctx'
alias kta='kti all'
alias kts='kti ss'

# --- Tab completion ---
_kt_completion() {
  local -a commands
  commands=(
    'context:Show current context'
    'ctx:Show current context (alias)'
    'info:Show module info'
    'find:Find files by pattern'
    'f:Find files (alias)'
    'here:Find files in current module'
    'h:Find files in current module (alias)'
    'ctx-find:Context-aware file find'
    'cf:Context-aware file find (alias)'
    'all:Find files in all modules'
    'a:Find files in all modules (alias)'
    'sourceset:Browse source sets'
    'ss:Browse source sets (alias)'
    'modules:List all modules'
    'mod:List all modules (alias)'
    'm:List all modules (alias)'
    'goto:Jump to module directory'
    'g:Jump to module (alias)'
    'tasks:Get Gradle tasks'
    'deps:Get module dependencies'
    'cache:Generate project cache'
  )

  _describe 'kt commands' commands
}

compdef _kt_completion kt

_kti_completion() {
  local -a commands
  commands=(
    'find:Find files by pattern (interactive)'
    'f:Find files (alias)'
    'here:Find files in current module (interactive)'
    'h:Find files in current module (alias)'
    'ctx:Context-aware file find (interactive)'
    'ctx-find:Context-aware file find (alias)'
    'cf:Context-aware file find (alias)'
    'all:Find files in all modules (interactive)'
    'a:Find files in all modules (alias)'
    'sourceset:Browse source sets (interactive)'
    'ss:Browse source sets (alias)'
    'modules:Browse modules (interactive)'
    'mod:Browse modules (alias)'
    'm:Browse modules (alias)'
  )

  _describe 'kti commands' commands
}

compdef _kti_completion kti

# --- Help function ---
kt-help() {
  cat <<'EOF'
kt - Kotlin Multiplatform Project Navigator

BASIC USAGE:
  kt <command>               Direct CLI usage (pipeable output)
  kti <command>              Interactive mode (uses fzf + editor)
  ktg [module]               Jump to module directory

COMMANDS:
  Context & Info:
    kt context               Show current context (module, source set, platform)
    kt info [MODULE]         Show module details (JSON)
    kt tasks [MODULE]        List Gradle tasks for module
    kt deps [MODULE]         List module dependencies

  File Navigation:
    kt find <pattern>        Find files matching pattern
    kt here                  List files in current module
    kt ctx-find              List files (prioritized by context)
    kt all                   List all Kotlin files
    kt sourceset [NAME]      List/browse source sets

    Add --interactive or -i to any file command for fzf selection

  Module Navigation:
    kt modules               List all modules
    kt goto [MODULE]         Print module path (or select with fzf)

  Cache:
    kt cache                 Regenerate project structure cache

INTERACTIVE SHORTCUTS (kti):
  kti find Recipe            Find + select + open Recipe files
  kti here                   Browse current module with fzf
  kti ctx                    Context-aware browse
  kti ss                     Select source set, then browse
  kti mod                    Browse modules, show info
  ktg                        Select module and cd to it
  ktg :backend               Jump directly to backend module

EXAMPLES:
  # Show where you are
  kt context

  # Find and open recipe files (interactive)
  kti find Recipe

  # Browse current module
  kti here

  # Jump to backend
  ktg :backend

  # List all modules (pipe to other tools)
  kt modules | grep frontend

  # Get tasks for current module
  kt tasks

  # Search in current module files
  kt here | xargs rg "suspend fun"

For more help: kt --help
EOF
}

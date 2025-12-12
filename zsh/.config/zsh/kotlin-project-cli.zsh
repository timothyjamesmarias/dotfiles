# --- Kotlin Multiplatform Project Navigation (Rust CLI wrapper) ---
# Shell integration for the kt Rust CLI tool with built-in fzf support

# Path to the kt binary
# Priority:
#   1. KT_BIN environment variable (if set)
#   2. System-installed binary (in PATH)
#   3. User-installed binary (~/.local/bin/kt)
#   4. Development version (~/projects/kt/target/release/kt)
#   5. Legacy location (~/bin/kt)

__find_kt_binary() {
  # Check if KT_BIN is already set and valid
  if [[ -n "$KT_BIN" ]] && [[ -x "$KT_BIN" ]]; then
    return 0
  fi

  # Check if kt is in PATH (installed via package manager or install-deps)
  # Use whence to get the actual path, not just the command name
  local kt_path=$(whence -p kt 2>/dev/null)
  if [[ -n "$kt_path" ]] && [[ -x "$kt_path" ]]; then
    KT_BIN="$kt_path"
    return 0
  fi

  # Check ~/.local/bin (installed via install-deps --kotlin)
  if [[ -x "$HOME/.local/bin/kt" ]]; then
    KT_BIN="$HOME/.local/bin/kt"
    return 0
  fi

  # Check development version
  if [[ -x "$HOME/projects/kt/target/release/kt" ]]; then
    KT_BIN="$HOME/projects/kt/target/release/kt"
    return 0
  fi

  # Check legacy location
  if [[ -x "$HOME/bin/kt" ]]; then
    KT_BIN="$HOME/bin/kt"
    return 0
  fi

  # Not found
  echo "Warning: kt binary not found." >&2
  echo "Install with: install-deps --kotlin" >&2
  echo "Or build from source: https://github.com/falki-io/kt" >&2
  return 1
}

# Find kt binary on load
if ! __find_kt_binary; then
  return 1
fi

# --- Main kt command (pass-through) ---
kt() {
  "$KT_BIN" "$@"
}

# --- Interactive wrapper functions ---
# These use the --interactive flag for a better terminal experience

# Interactive wrapper for kt commands (adds fzf integration)
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
    sym|symbol)
      if [[ -z "$1" ]]; then
        echo "Usage: kti symbol <query>"
        return 1
      fi
      "$KT_BIN" symbol "$@" --interactive
      ;;
    expect|exp)
      "$KT_BIN" expect --interactive
      ;;
    # Spring Boot commands
    spring-components|spc)
      "$KT_BIN" spring components "$@" --interactive
      ;;
    spring-endpoints|spe)
      "$KT_BIN" spring endpoints --interactive
      ;;
    spring-entities|spent)
      "$KT_BIN" spring entities --interactive
      ;;
    *)
      echo "Unknown command: $cmd"
      echo "Usage: kti {find|here|ctx|all|sourceset|modules|symbol|expect|spring-components|spring-endpoints|spring-entities}"
      echo ""
      echo "Examples:"
      echo "  kti find Recipe    # Find and open Recipe files with fzf"
      echo "  kti here           # Browse current module files with fzf"
      echo "  kti ctx            # Context-aware browse with fzf"
      echo "  kti ss             # Select source set, then browse"
      echo "  kti mod            # Browse modules and show info"
      echo "  kti sym Recipe     # Search for Recipe symbols and jump to them"
      echo "  kti expect         # Browse expect declarations and their actuals"
      echo ""
      echo "Spring Boot commands:"
      echo "  kti spc            # Browse Spring components (controllers, services, etc.)"
      echo "  kti spe            # Browse REST endpoints"
      echo "  kti spent          # Browse JPA entities"
      return 1
      ;;
  esac
}

# Jump to module directory with cd (interactive if no arg)
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
alias ktsym='kti sym'        # Search and jump to symbols
alias ktexp='kti expect'     # Browse expect/actual declarations

# Spring Boot aliases
alias ktspc='kti spc'        # Browse Spring components
alias ktspe='kti spe'        # Browse REST endpoints
alias ktspent='kti spent'    # Browse JPA entities

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
    'symbol:Search for symbols (classes, functions, etc.)'
    'sym:Search for symbols (alias)'
    'expect:Find expect/actual declarations'
    'actual:Find expect for current actual'
    'spring:Spring Boot commands'
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
    'symbol:Search for symbols (interactive)'
    'sym:Search for symbols (alias)'
    'expect:Browse expect/actual declarations (interactive)'
    'exp:Browse expect/actual declarations (alias)'
    'spring-components:Browse Spring components (interactive)'
    'spc:Browse Spring components (alias)'
    'spring-endpoints:Browse REST endpoints (interactive)'
    'spe:Browse REST endpoints (alias)'
    'spring-entities:Browse JPA entities (interactive)'
    'spent:Browse JPA entities (alias)'
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

  Symbol Search:
    kt symbol <query>        Search for symbols (classes, functions, etc.)
    kt expect                Find expect declarations and their actuals
    kt actual                Find expect for current actual

  Module Navigation:
    kt modules               List all modules
    kt goto [MODULE]         Print module path (or select with fzf)

  Spring Boot:
    kt spring components     List Spring components (@RestController, @Service, etc.)
    kt spring endpoints      List REST endpoints
    kt spring entities       List JPA entities

  Cache:
    kt cache                 Regenerate project structure cache

INTERACTIVE SHORTCUTS (kti):
  kti find Recipe            Find + select + open Recipe files
  kti here                   Browse current module with fzf
  kti ctx                    Context-aware browse
  kti ss                     Select source set, then browse
  kti mod                    Browse modules, show info
  kti sym Recipe             Search for symbols and jump to them
  kti expect                 Browse expect/actual declarations
  kti spc                    Browse Spring components
  kti spe                    Browse REST endpoints
  kti spent                  Browse JPA entities
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

  # Find all REST endpoints
  kt spring endpoints

  # Browse and open a Spring controller
  kti spc

For more help: kt --help
EOF
}

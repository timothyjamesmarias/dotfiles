# --- Kotlin Multiplatform Project Navigation ---
# Advanced tooling for navigating complex KMP projects with multiple modules and source sets

# --- Context Detection ---
# Detect current module from pwd
kt-current-module() {
  local current_path="$PWD"

  # Try to find the module by walking up the directory tree
  while [[ "$current_path" != "/" ]]; do
    # Check if there's a build.gradle.kts here
    if [[ -f "$current_path/build.gradle.kts" ]]; then
      # Get the relative path from project root
      local project_root=$(kt-project-root)
      if [[ -n "$project_root" ]]; then
        local rel_path="${current_path#$project_root/}"
        # Convert path to Gradle module notation (e.g., frontend/shared -> :frontend:shared)
        if [[ "$rel_path" == "$current_path" ]]; then
          # We're at root
          echo ":"
        else
          echo ":${rel_path//\//:}"
        fi
        return 0
      fi
    fi
    current_path="${current_path%/*}"
  done

  echo ""
  return 1
}

# Find project root (has settings.gradle.kts)
kt-project-root() {
  local current_path="$PWD"

  while [[ "$current_path" != "/" ]]; do
    if [[ -f "$current_path/settings.gradle.kts" ]] || [[ -f "$current_path/settings.gradle" ]]; then
      echo "$current_path"
      return 0
    fi
    current_path="${current_path%/*}"
  done

  echo ""
  return 1
}

# Detect current source set from pwd (commonMain, androidMain, etc.)
kt-current-sourceset() {
  local current_path="$PWD"

  # Look for src/{sourceSet}/kotlin pattern
  # In zsh, use match array instead of BASH_REMATCH
  if [[ "$current_path" =~ /src/([^/]+)/(kotlin|java|resources) ]]; then
    echo "${match[1]}"
    return 0
  fi

  echo ""
  return 1
}

# Detect platform from source set name
kt-current-platform() {
  local sourceset=$(kt-current-sourceset)

  if [[ -z "$sourceset" ]]; then
    echo ""
    return 1
  fi

  # Extract platform from source set name
  # commonMain -> common
  # androidMain -> android
  # iosX64Main -> ios
  # desktopTest -> desktop
  if [[ "$sourceset" =~ ^common ]]; then
    echo "common"
  elif [[ "$sourceset" =~ ^android ]]; then
    echo "android"
  elif [[ "$sourceset" =~ ^ios ]]; then
    echo "ios"
  elif [[ "$sourceset" =~ ^(desktop|jvm) ]]; then
    echo "desktop"
  elif [[ "$sourceset" =~ ^(js|web) ]]; then
    echo "js"
  else
    echo "$sourceset"
  fi

  return 0
}

# Display current context (for prompt or debugging)
kt-context() {
  local root=$(kt-project-root)
  local module=$(kt-current-module)
  local sourceset=$(kt-current-sourceset)
  local platform=$(kt-current-platform)

  if [[ -z "$root" ]]; then
    echo "Not in a Kotlin project"
    return 1
  fi

  echo "Project root: $root"
  [[ -n "$module" ]] && echo "Module: $module"
  [[ -n "$sourceset" ]] && echo "Source set: $sourceset"
  [[ -n "$platform" ]] && echo "Platform: $platform"
}

# Save context to temp file for other tools to use
kt-save-context() {
  local context_file="/tmp/kt-context-${$}"
  local module=$(kt-current-module)
  local sourceset=$(kt-current-sourceset)
  local platform=$(kt-current-platform)
  local root=$(kt-project-root)

  cat > "$context_file" <<EOF
KT_PROJECT_ROOT=$root
KT_MODULE=$module
KT_SOURCESET=$sourceset
KT_PLATFORM=$platform
EOF

  echo "$context_file"
}

# Load saved context
kt-load-context() {
  local context_file="${1:-/tmp/kt-context-${$}}"
  if [[ -f "$context_file" ]]; then
    source "$context_file"
  fi
}

# --- Module-Aware Navigation ---

# Find files in current module only
ktf-here() {
  local root=$(kt-project-root)
  local module=$(kt-current-module)

  if [[ -z "$root" ]]; then
    echo "Not in a Kotlin project"
    return 1
  fi

  # Convert module notation to path
  # :frontend:shared -> frontend/shared
  local module_path="${module#:}"
  module_path="${module_path//://}"

  local search_path="$root/$module_path"

  # Find Kotlin files in this module only, excluding build
  find "$search_path" -name "*.kt" -not -path "*/build/*" 2>/dev/null \
    | fzf --preview 'bat --style=numbers --color=always {}' \
    | xargs -r nvim
}

# Find files with context awareness (prioritize current source set and platform)
ktf-context() {
  local root=$(kt-project-root)
  local module=$(kt-current-module)
  local sourceset=$(kt-current-sourceset)
  local platform=$(kt-current-platform)

  if [[ -z "$root" ]]; then
    echo "Not in a Kotlin project"
    return 1
  fi

  # Convert module notation to path
  local module_path="${module#:}"
  module_path="${module_path//://}"
  local search_path="$root/$module_path"

  # Build search paths prioritized by context
  local search_paths=()

  if [[ -n "$sourceset" ]]; then
    # Prioritize current source set
    search_paths+=("$search_path/src/$sourceset")
  fi

  if [[ -n "$platform" && "$platform" != "common" ]]; then
    # Add platform-specific source sets (e.g., androidMain, androidTest)
    search_paths+=("$search_path/src/${platform}Main")
    search_paths+=("$search_path/src/${platform}Test")
  fi

  # Always include commonMain and commonTest
  search_paths+=("$search_path/src/commonMain")
  search_paths+=("$search_path/src/commonTest")

  # Add the whole module as fallback
  search_paths+=("$search_path")

  # Find files in prioritized order
  local files=()
  for path in "${search_paths[@]}"; do
    if [[ -d "$path" ]]; then
      while IFS= read -r file; do
        files+=("$file")
      done < <(find "$path" -name "*.kt" -not -path "*/build/*" 2>/dev/null)
    fi
  done

  # Remove duplicates while preserving order (first occurrence = highest priority)
  local unique_files=()
  local seen=()

  for file in "${files[@]}"; do
    if [[ ! " ${seen[@]} " =~ " ${file} " ]]; then
      unique_files+=("$file")
      seen+=("$file")
    fi
  done

  # Use fzf to select, with preview
  printf '%s\n' "${unique_files[@]}" \
    | fzf --preview 'bat --style=numbers --color=always {}' \
    | xargs -r nvim
}

# Find files across all modules (global search)
ktf-all() {
  local root=$(kt-project-root)

  if [[ -z "$root" ]]; then
    echo "Not in a Kotlin project"
    return 1
  fi

  find "$root" -name "*.kt" -not -path "*/build/*" -not -path "*/.gradle/*" 2>/dev/null \
    | fzf --preview 'bat --style=numbers --color=always {}' \
    | xargs -r nvim
}

# Search for Kotlin files by name pattern
ktf-find() {
  local pattern="$1"
  local root=$(kt-project-root)

  if [[ -z "$root" ]]; then
    echo "Not in a Kotlin project"
    return 1
  fi

  if [[ -z "$pattern" ]]; then
    echo "Usage: ktf-find <pattern>"
    return 1
  fi

  find "$root" -name "*${pattern}*.kt" -not -path "*/build/*" -not -path "*/.gradle/*" 2>/dev/null \
    | fzf --preview 'bat --style=numbers --color=always {}' \
    | xargs -r nvim
}

# Jump to a specific source set in current module
ktf-sourceset() {
  local root=$(kt-project-root)
  local module=$(kt-current-module)

  if [[ -z "$root" || -z "$module" ]]; then
    echo "Not in a module"
    return 1
  fi

  # Convert module notation to path
  local module_path="${module#:}"
  module_path="${module_path//://}"
  local search_path="$root/$module_path"

  # Find all source sets
  local sourceset=$(find "$search_path/src" -mindepth 1 -maxdepth 1 -type d 2>/dev/null \
    | xargs -n1 basename \
    | fzf --prompt="Select source set: ")

  if [[ -z "$sourceset" ]]; then
    return 1
  fi

  # Find files in that source set
  find "$search_path/src/$sourceset" -name "*.kt" 2>/dev/null \
    | fzf --preview 'bat --style=numbers --color=always {}' \
    | xargs -r nvim
}

# --- Project Structure Cache ---

# Generate project model cache (JSON file with module metadata)
kt-cache-project() {
  local root=$(kt-project-root)

  if [[ -z "$root" ]]; then
    echo "Not in a Kotlin project"
    return 1
  fi

  local cache_file="$root/.kt-project-cache.json"
  echo "Generating project structure cache..."

  # Get all modules from Gradle
  local modules_raw=$(cd "$root" && ./gradlew -q projects 2>/dev/null)

  # Parse modules into JSON array
  local modules_json="["
  local first=true

  while IFS= read -r line; do
    # Match lines like "+--- Project ':backend'" or "\--- Project ':frontend:shared'"
    if [[ "$line" =~ Project\ \'([^\']+)\' ]]; then
      local module_name="${match[1]}"

      # Get module path
      local mod_path="${module_name#:}"
      mod_path="${mod_path//://}"

      if [[ -z "$mod_path" ]]; then
        mod_path="."
      fi

      # Find source sets in this module
      local source_sets=()
      if [[ -d "$root/$mod_path/src" ]]; then
        while IFS= read -r sourceset_dir; do
          source_sets+=("$(basename "$sourceset_dir")")
        done < <(find "$root/$mod_path/src" -mindepth 1 -maxdepth 1 -type d 2>/dev/null)
      fi

      # Build source sets JSON array
      local source_sets_json="["
      local first_ss=true
      for ss in "${source_sets[@]}"; do
        if [[ "$first_ss" == true ]]; then
          first_ss=false
        else
          source_sets_json+=","
        fi
        source_sets_json+="\"$ss\""
      done
      source_sets_json+="]"

      # Detect module type (KMP vs JVM)
      local module_type="jvm"
      if [[ -f "$root/$mod_path/build.gradle.kts" ]]; then
        if grep -q "kotlin(\"multiplatform\")" "$root/$mod_path/build.gradle.kts" 2>/dev/null; then
          module_type="multiplatform"
        fi
      fi

      # Add module to JSON
      if [[ "$first" == true ]]; then
        first=false
      else
        modules_json+=","
      fi

      modules_json+=$(cat <<EOF

{
  "name": "$module_name",
  "path": "$mod_path",
  "type": "$module_type",
  "sourceSets": $source_sets_json
}
EOF
)
    fi
  done <<< "$modules_raw"

  modules_json+=$'\n]'

  # Write full JSON structure
  cat > "$cache_file" <<EOF
{
  "root": "$root",
  "generatedAt": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "modules": $modules_json
}
EOF

  echo "Project cache generated: $cache_file"
  echo "Modules found: $(echo "$modules_json" | grep -c "\"name\":")"
}

# List all modules from cache (or generate if missing)
kt-modules() {
  local root=$(kt-project-root)

  if [[ -z "$root" ]]; then
    echo "Not in a Kotlin project"
    return 1
  fi

  local cache_file="$root/.kt-project-cache.json"

  # Generate cache if it doesn't exist
  if [[ ! -f "$cache_file" ]]; then
    echo "Cache not found, generating..."
    kt-cache-project
  fi

  # Use jq to extract module names
  if command -v jq &> /dev/null; then
    jq -r '.modules[].name' "$cache_file"
  else
    # Fallback without jq
    grep '"name"' "$cache_file" | sed 's/.*"name": "\([^"]*\)".*/\1/'
  fi
}

# Interactive module selector
kt-module-select() {
  local root=$(kt-project-root)

  if [[ -z "$root" ]]; then
    echo "Not in a Kotlin project"
    return 1
  fi

  kt-modules | fzf --prompt="Select module: "
}

# Jump to module directory
kt-goto-module() {
  local root=$(kt-project-root)
  local module="${1:-$(kt-module-select)}"

  if [[ -z "$root" || -z "$module" ]]; then
    return 1
  fi

  local cache_file="$root/.kt-project-cache.json"

  # Get module path from cache
  local module_path
  if command -v jq &> /dev/null; then
    module_path=$(jq -r ".modules[] | select(.name==\"$module\") | .path" "$cache_file")
  else
    # Fallback: convert module name to path
    module_path="${module#:}"
    module_path="${module_path//://}"
  fi

  if [[ -z "$module_path" || "$module_path" == "null" ]]; then
    echo "Module not found: $module"
    return 1
  fi

  cd "$root/$module_path"
}

# Show module info
kt-module-info() {
  local root=$(kt-project-root)
  local module="${1:-$(kt-current-module)}"

  if [[ -z "$root" || -z "$module" ]]; then
    echo "Not in a module"
    return 1
  fi

  local cache_file="$root/.kt-project-cache.json"

  if [[ ! -f "$cache_file" ]]; then
    kt-cache-project
  fi

  if command -v jq &> /dev/null; then
    jq ".modules[] | select(.name==\"$module\")" "$cache_file"
  else
    echo "jq not installed, cannot display module info"
    return 1
  fi
}

# --- Unified kt() Command Wrapper ---

kt() {
  local command="$1"
  shift

  case "$command" in
    # Context commands
    context|ctx)
      kt-context
      ;;
    info)
      kt-module-info "$@"
      ;;

    # Navigation commands
    find|f)
      ktf-find "$@"
      ;;
    here|h)
      ktf-here
      ;;
    ctx-find|cf)
      ktf-context
      ;;
    all|a)
      ktf-all
      ;;
    ss|sourceset)
      ktf-sourceset
      ;;

    # Module commands
    modules|mod|m)
      kt-modules
      ;;
    goto|g)
      kt-goto-module "$@"
      ;;

    # Cache commands
    cache)
      kt-cache-project
      ;;

    # Help
    help|--help|-h|"")
      cat <<'EOF'
kt - Kotlin Multiplatform Project Navigator

CONTEXT COMMANDS:
  kt context       Show current context (module, source set, platform)
  kt info [MOD]    Show detailed module info (defaults to current)

NAVIGATION COMMANDS:
  kt find <PAT>    Find Kotlin files by name pattern
  kt here          Find files in current module
  kt ctx-find      Find files with context awareness (prioritizes current platform)
  kt all           Find files across all modules
  kt sourceset     Browse and select from available source sets

MODULE COMMANDS:
  kt modules       List all modules in project
  kt goto [MOD]    Jump to module directory (interactive if no module specified)

CACHE COMMANDS:
  kt cache         Generate/refresh project structure cache

EXAMPLES:
  kt context                      # Show where you are in the project
  kt find Recipe                  # Find files containing "Recipe"
  kt here                         # Browse files in current module
  kt goto :backend                # Jump to backend module
  kt ctx-find                     # Find files (prioritized by current platform)

ALIASES:
  context -> ctx
  find -> f
  here -> h
  ctx-find -> cf
  all -> a
  sourceset -> ss
  modules -> mod, m
  goto -> g
EOF
      ;;

    *)
      echo "Unknown command: $command"
      echo "Run 'kt help' for usage"
      return 1
      ;;
  esac
}

# Auto-completion for kt command
_kt_completion() {
  local -a commands
  commands=(
    'context:Show current context'
    'ctx:Show current context'
    'info:Show module info'
    'find:Find files by pattern'
    'f:Find files by pattern'
    'here:Find files in current module'
    'h:Find files in current module'
    'ctx-find:Context-aware file find'
    'cf:Context-aware file find'
    'all:Find files in all modules'
    'a:Find files in all modules'
    'sourceset:Browse source sets'
    'ss:Browse source sets'
    'modules:List all modules'
    'mod:List all modules'
    'm:List all modules'
    'goto:Jump to module'
    'g:Jump to module'
    'cache:Generate project cache'
    'help:Show help'
  )

  _describe 'kt commands' commands
}

compdef _kt_completion kt

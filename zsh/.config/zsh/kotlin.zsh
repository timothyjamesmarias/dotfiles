# --- Kotlin/Gradle utilities ---
# Terminal workflow for JVM projects

# --- Gradle wrapper detection ---
gw() {
  if [ -f "./gradlew" ]; then
    ./gradlew "$@"
  elif [ -f "../gradlew" ]; then
    ../gradlew "$@"
  else
    echo "No gradlew found in current or parent directory"
    return 1
  fi
}

# --- Common Gradle tasks ---
alias gwb='gw build'
alias gwc='gw clean'
alias gwcb='gw clean build'
alias gwt='gw test'
alias gwr='gw run'
alias gwi='gw --info'
alias gwq='gw -q'                    # Quiet mode
alias gwd='gw dependencies'
alias gwdt='gw dependencyTree'
alias gwtasks='gw tasks --all'
alias gwprops='gw properties'

# --- Run specific test ---
# Usage: gwtest TestClassName
# Usage: gwtest TestClassName testMethodName
gwtest() {
  local class="$1"
  local method="$2"

  if [ -z "$class" ]; then
    # Interactive: find test files and select
    local test_file=$(find . -name "*Test.kt" -o -name "*Test.java" 2>/dev/null \
      | fzf --preview 'bat --style=numbers --color=always {}')
    [ -z "$test_file" ] && return

    # Extract class name from file
    class=$(basename "$test_file" | sed 's/\.[^.]*$//')
  fi

  if [ -n "$method" ]; then
    gw test --tests "$class.$method"
  else
    gw test --tests "$class"
  fi
}

# --- Run test at cursor (for integration with neovim) ---
# Expects: file path and optional line number
gwtest-at() {
  local file="$1"
  local line="${2:-1}"

  if [ -z "$file" ]; then
    echo "Usage: gwtest-at <file> [line]"
    return 1
  fi

  # Extract class name from file
  local class=$(basename "$file" | sed 's/\.[^.]*$//')

  # Try to find method name at line (basic heuristic)
  local method=$(sed -n "${line}p" "$file" 2>/dev/null | grep -oE 'fun `?[^`(]+' | sed 's/fun `\?//')

  if [ -n "$method" ]; then
    echo "Running: $class.$method"
    gw test --tests "$class.$method" --info
  else
    echo "Running: $class"
    gw test --tests "$class" --info
  fi
}

# --- Continuous build ---
alias gww='gw build --continuous'
alias gwtw='gw test --continuous'

# --- Kotlin compilation ---
alias gwkc='gw compileKotlin'
alias gwktc='gw compileTestKotlin'

# --- Spring Boot specific ---
alias gwboot='gw bootRun'
alias gwbootd='gw bootRun --debug-jvm'
alias gwbootjar='gw bootJar'

# --- Project info ---
gwmodules() {
  gw projects 2>/dev/null | grep -E "^[+\\]---" | sed 's/[+\\]--- //'
}

# --- Find and run main class ---
gwmain() {
  local main_file=$(find . -name "*.kt" -exec grep -l "fun main" {} \; 2>/dev/null \
    | fzf --preview 'bat --style=numbers --color=always {}')
  [ -z "$main_file" ] && return

  local class=$(grep -E "^package " "$main_file" | sed 's/package //' | tr -d '\r')
  local filename=$(basename "$main_file" .kt)

  if [ -n "$class" ]; then
    echo "Running: ${class}.${filename}Kt"
    gw run -PmainClass="${class}.${filename}Kt"
  else
    echo "Running: ${filename}Kt"
    gw run -PmainClass="${filename}Kt"
  fi
}

# --- Dependency search ---
gwdep() {
  local query="$1"
  if [ -z "$query" ]; then
    gw dependencies | fzf
  else
    gw dependencies | grep -i "$query"
  fi
}

# --- Run tasks with device selection ---
# Interactive task runner (uses kt CLI with fzf)
gwrun() {
  kt run
}

# Android specific
gwandroid() {
  kt run installDebug
}

# iOS specific (requires macOS)
gwios() {
  local task=$(gw tasks --all 2>/dev/null | grep -i "iosSimulator" | fzf | awk '{print $1}')
  [ -n "$task" ] && kt run "$task"
}

# --- Quick project navigation ---
# Jump to source directories
ktsrc() {
  local src=$(find . -type d -name "kotlin" -path "*/src/*" 2>/dev/null \
    | fzf --preview 'tree -C -L 2 {}')
  [ -n "$src" ] && cd "$src"
}

kttest() {
  local test=$(find . -type d -name "kotlin" -path "*/test/*" 2>/dev/null \
    | fzf --preview 'tree -C -L 2 {}')
  [ -n "$test" ] && cd "$test"
}

# --- Debug helpers ---
# Attach debugger (prints the command for IDEA or DAP)
gwdebug() {
  local port="${1:-5005}"
  echo "Starting Gradle with debug agent on port $port"
  echo "Attach with: -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:$port"
  gw run --debug-jvm
}

# --- Build scan ---
alias gwscan='gw build --scan'

# --- Refresh dependencies ---
alias gwrefresh='gw build --refresh-dependencies'
alias gwcache='gw clean && rm -rf ~/.gradle/caches/modules-2/files-2.1'

# --- Generate ctags for Kotlin ---
kttags() {
  ctags -R --languages=Kotlin --exclude=build --exclude=.gradle .
}

# Note: ktf, ktclass, and other kt* navigation functions are now provided
# by the kt Rust CLI (see kotlin-project-cli.zsh)

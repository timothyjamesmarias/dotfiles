# --- Gradle & Kotlin dev aliases ---

# Core Gradle wrapper aliases
alias gw='./gradlew'
alias gwb='./gradlew build'
alias gwc='./gradlew clean'
alias gwt='./gradlew test'
alias gwbt='./gradlew build -x test'
alias gwr='./gradlew run'
alias gwcb='./gradlew clean build'

# Spring Boot specific
alias gwbs='./gradlew bootRun'
alias gwbsd='./gradlew bootRun --args="--spring.profiles.active=dev"'
alias gwbst='./gradlew bootTest'
alias gwjar='./gradlew bootJar'

# Kotlin Multiplatform
alias gwkjs='./gradlew jsRun'
alias gwkjvm='./gradlew jvmRun'
alias gwknative='./gradlew nativeRun'
alias gwkall='./gradlew allTests'
alias gwkbrowser='./gradlew jsBrowserRun'

# Testing
alias gwtw='./gradlew test --continuous'
alias gwtf='./gradlew test --tests'
alias gwti='./gradlew integrationTest'

# Dev workflows
alias gwdev='./gradlew bootRun --continuous'
alias gwdebug='./gradlew bootRun --debug-jvm'
alias gwscan='./gradlew build --scan'

# Dependencies
alias gwdeps='./gradlew dependencies'
alias gwdepup='./gradlew dependencyUpdates'

# --- Functions ---

# Interactive task selector with fzf
gwf() {
  local task=$(./gradlew tasks --all | grep -E "^[a-z]" | awk '{print $1}' | sort -u | fzf --preview './gradlew help --task {1} 2>/dev/null || echo "No help available for {1}"')
  if [ -n "$task" ]; then
    echo "Running: ./gradlew $task"
    ./gradlew $task
  fi
}

# List all Gradle tasks with filtering
gwl() {
  if [ -z "$1" ]; then
    ./gradlew tasks --all
  else
    ./gradlew tasks --all | grep -i "$1"
  fi
}

# Quick clean and build
gwcbt() {
  ./gradlew clean build -x test
}

# Run specific test class
gwtest() {
  if [ -z "$1" ]; then
    echo "Usage: gwtest <TestClassName>"
    return 1
  fi
  ./gradlew test --tests "*$1*"
}

# Run Gradle with performance profiling
gwprof() {
  ./gradlew "$@" --profile --scan
}

# Create gradle.properties if it doesn't exist
gwinit() {
  if [ -f "gradle.properties" ]; then
    echo "gradle.properties already exists"
    return 1
  fi

  cat > gradle.properties << 'EOF'
# Gradle performance settings
org.gradle.daemon=true
org.gradle.parallel=true
org.gradle.caching=true
org.gradle.configureondemand=true
org.gradle.jvmargs=-Xmx4g -XX:MaxMetaspaceSize=1g -XX:+HeapDumpOnOutOfMemoryError

# Kotlin compiler settings
kotlin.incremental=true
kotlin.incremental.js=true
kotlin.caching.enabled=true
kotlin.daemon.jvmargs=-Xmx2g

# Kotlin compiler warnings
kotlin.compiler.suppressWarnings=false
EOF

  echo "Created gradle.properties with performance optimizations"
}

# Find and edit Kotlin file with fzf
kf() {
  local file=$(fd -e kt -e kts | fzf --preview 'bat --style=numbers --color=always {}')
  if [ -n "$file" ]; then
    $EDITOR "$file"
  fi
}

# Search Kotlin files for pattern
kgrep() {
  if [ -z "$1" ]; then
    echo "Usage: kgrep <pattern>"
    return 1
  fi
  rg "$1" --type kotlin -C 3
}

# Java version management (if using SDKMAN)
jvl() {
  sdk list java
}

jvu() {
  sdk use java "$1"
}

jvi() {
  sdk install java "$1"
}

# Quick wrapper check
gwv() {
  if [ -f "./gradlew" ]; then
    ./gradlew --version
  else
    echo "No Gradle wrapper found in current directory"
  fi
}

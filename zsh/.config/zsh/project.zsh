# project.zsh - Per-project configuration utilities
# Reads from .project.conf (KEY=VALUE format) in project root

# Read a value from .project.conf, walking up parent directories
_project_conf_get() {
    local key="$1"
    local dir="$PWD"

    while [[ "$dir" != "/" ]]; do
        if [[ -f "$dir/.project.conf" ]]; then
            grep -m1 "^${key}=" "$dir/.project.conf" | cut -d= -f2- | sed 's/^"//;s/"$//'
            return 0
        fi
        dir="$(dirname "$dir")"
    done
    return 1
}

# Open current project in the configured JetBrains IDE
jb() {
    local ide
    ide=$(_project_conf_get ide)

    if [[ -z "$ide" ]]; then
        echo "No 'ide' key found in .project.conf"
        echo "Create one with: project-init <ide>"
        echo "Valid IDEs: idea webstorm pycharm clion goland rider rubymine phpstorm datagrip fleet"
        return 1
    fi

    local valid_ides=(idea webstorm pycharm clion goland rider rubymine phpstorm datagrip fleet)
    if (( ! ${valid_ides[(Ie)$ide]} )); then
        echo "Unknown IDE: $ide"
        echo "Valid IDEs: ${valid_ides[*]}"
        return 1
    fi

    if ! command -v "$ide" &>/dev/null; then
        echo "Command '$ide' not found. Is the JetBrains Toolbox CLI installed?"
        return 1
    fi

    "$ide" "${1:-.}"
}

# Initialize .project.conf in current directory
project-init() {
    if [[ -f ".project.conf" ]]; then
        echo ".project.conf already exists:"
        cat .project.conf
        return 0
    fi

    local ide="${1:-}"
    if [[ -z "$ide" ]]; then
        echo "Usage: project-init <ide>"
        echo "Valid IDEs: idea webstorm pycharm clion goland rider rubymine phpstorm datagrip fleet"
        return 1
    fi

    local valid_ides=(idea webstorm pycharm clion goland rider rubymine phpstorm datagrip fleet)
    if (( ! ${valid_ides[(Ie)$ide]} )); then
        echo "Unknown IDE: $ide"
        echo "Valid IDEs: ${valid_ides[*]}"
        return 1
    fi

    echo "# .project.conf" > .project.conf
    echo "ide=$ide" >> .project.conf
    echo "Created .project.conf with ide=$ide"
}

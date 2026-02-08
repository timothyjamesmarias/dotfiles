#!/usr/bin/env bash
# YAML parser for workspace profiles
# Provides functions to extract values from YAML files

# Parse YAML file and extract a specific value
# Usage: yaml_get_value <file> <key_path>
# Example: yaml_get_value profile.yaml "name"
# Example: yaml_get_value profile.yaml "tmux.windows[0].name"
yaml_get_value() {
    local file="$1"
    local key_path="$2"

    if [[ ! -f "$file" ]]; then
        echo "Error: File not found: $file" >&2
        return 1
    fi

    # Simple YAML parser using awk
    # This handles basic YAML structures: scalars, lists, and nested keys
    awk -v key="$key_path" '
    BEGIN {
        depth = 0
        in_list = 0
        list_index = 0
    }

    # Track indentation depth
    /^[[:space:]]*[a-zA-Z_-]+:/ {
        match($0, /^[[:space:]]*/);
        current_depth = RLENGTH / 2

        # Extract key and value
        sub(/^[[:space:]]*/, "")
        split($0, kv, /:/)
        current_key = kv[1]
        value = kv[2]
        sub(/^[[:space:]]*/, "", value)

        # Build full path
        if (current_depth == 0) {
            path[0] = current_key
            depth = 0
        } else {
            path[current_depth] = current_key
            depth = current_depth
        }

        # Check if this matches our search key
        full_path = path[0]
        for (i = 1; i <= depth; i++) {
            full_path = full_path "." path[i]
        }

        if (full_path == key && value != "") {
            print value
            exit
        }
    }

    # Handle list items
    /^[[:space:]]*-[[:space:]]/ {
        if (match($0, /^[[:space:]]*-[[:space:]]/)) {
            indent = RLENGTH
            value = substr($0, indent + 1)
            print value
        }
    }
    ' "$file"
}

# Get array of values matching a pattern
# Usage: yaml_get_array <file> <pattern>
# Example: yaml_get_array profile.yaml "detect"
yaml_get_array() {
    local file="$1"
    local pattern="$2"

    if [[ ! -f "$file" ]]; then
        echo "Error: File not found: $file" >&2
        return 1
    fi

    awk -v pat="$pattern" '
    BEGIN { in_section = 0 }

    # Match the section header
    $1 == pat":" {
        in_section = 1
        next
    }

    # If in section and line starts with -, extract value
    in_section && /^[[:space:]]*-/ {
        # Extract the value after the dash
        match($0, /^[[:space:]]*-[[:space:]]*/)
        value = substr($0, RLENGTH + 1)
        # Remove quotes if present
        gsub(/^["'\'']|["'\'']$/, "", value)
        print value
    }

    # Exit section when we hit a non-indented line
    in_section && /^[a-zA-Z_-]+:/ {
        in_section = 0
    }
    ' "$file"
}

# Check if a key exists in the YAML file
# Usage: yaml_has_key <file> <key>
yaml_has_key() {
    local file="$1"
    local key="$2"

    if [[ ! -f "$file" ]]; then
        return 1
    fi

    grep -q "^${key}:" "$file"
}

# Extract all window definitions from tmux section
# Returns window count
# Usage: yaml_get_tmux_windows <file>
yaml_get_tmux_window_count() {
    local file="$1"

    awk '
    BEGIN {
        in_windows = 0
        count = 0
    }

    /^[[:space:]]*windows:/ {
        in_windows = 1
        next
    }

    in_windows && /^[[:space:]]*-[[:space:]]*name:/ {
        count++
    }

    # Exit windows section when we hit a top-level key (no leading spaces before key)
    in_windows && /^[a-zA-Z_-]+:/ {
        in_windows = 0
    }

    END { print count }
    ' "$file"
}

# Extract window name by index
# Usage: yaml_get_window_name <file> <index>
yaml_get_window_name() {
    local file="$1"
    local index="$2"

    awk -v idx="$index" '
    BEGIN {
        in_windows = 0
        current_window = -1
    }

    /^[[:space:]]*windows:/ {
        in_windows = 1
        next
    }

    in_windows && /^[[:space:]]*-[[:space:]]*name:/ {
        current_window++
        if (current_window == idx) {
            sub(/^[[:space:]]*-[[:space:]]*name:[[:space:]]*/, "")
            gsub(/["'\'']/, "")
            print
            exit
        }
    }
    ' "$file"
}

# Extract window layout by index
# Usage: yaml_get_window_layout <file> <index>
yaml_get_window_layout() {
    local file="$1"
    local index="$2"

    awk -v idx="$index" '
    BEGIN {
        in_windows = 0
        current_window = -1
        in_target_window = 0
    }

    /^[[:space:]]*windows:/ {
        in_windows = 1
        next
    }

    in_windows && /^[[:space:]]*-[[:space:]]*name:/ {
        current_window++
        if (current_window == idx) {
            in_target_window = 1
        } else {
            in_target_window = 0
        }
        next
    }

    in_target_window && /^[[:space:]]*layout:/ {
        sub(/^[[:space:]]*layout:[[:space:]]*/, "")
        gsub(/["'\'']/, "")
        print
        exit
    }

    # Stop when we hit the next window or end of windows section
    in_windows && in_target_window && /^[[:space:]]*-[[:space:]]*name:/ {
        in_target_window = 0
    }
    ' "$file"
}

# Extract pane commands for a window by index
# Returns one command per line
# Usage: yaml_get_window_panes <file> <index>
yaml_get_window_panes() {
    local file="$1"
    local index="$2"

    awk -v idx="$index" '
    BEGIN {
        in_windows = 0
        current_window = -1
        in_target_window = 0
        in_panes = 0
    }

    /^[[:space:]]*windows:/ {
        in_windows = 1
        next
    }

    in_windows && /^[[:space:]]*-[[:space:]]*name:/ {
        current_window++
        if (current_window == idx) {
            in_target_window = 1
        } else {
            in_target_window = 0
            in_panes = 0
        }
        next
    }

    in_target_window && /^[[:space:]]*panes:/ {
        in_panes = 1
        next
    }

    in_panes && /^[[:space:]]*-[[:space:]]*\{?cmd:/ {
        # Extract command
        line = $0
        sub(/^[[:space:]]*-[[:space:]]*\{?cmd:[[:space:]]*/, "", line)
        gsub(/["'\''\{\}]/, "", line)
        sub(/\}$/, "", line)
        print line
    }

    # Stop when we hit the next window or non-pane content
    in_panes && /^[[:space:]]*-[[:space:]]*name:/ {
        in_panes = 0
        in_target_window = 0
    }

    in_panes && /^[[:space:]]*[a-zA-Z_-]+:/ && !/cmd:/ {
        in_panes = 0
    }
    ' "$file"
}

# Validate profile file
# Usage: yaml_validate_profile <file>
yaml_validate_profile() {
    local file="$1"

    if [[ ! -f "$file" ]]; then
        echo "Error: Profile file not found: $file" >&2
        return 1
    fi

    # Check for required fields
    if ! yaml_has_key "$file" "name"; then
        echo "Error: Profile missing required field: name" >&2
        return 1
    fi

    return 0
}

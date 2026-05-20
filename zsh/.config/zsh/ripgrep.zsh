# ripgrep.zsh - Ripgrep configuration and per-project ignore setup

export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

_RGIGNORE_TEMPLATES="$HOME/dotfiles/ripgrep/templates"

# Theme dir names treated as defaults (filtered out of candidate suggestions)
_RGIGNORE_DEFAULT_THEME_PATTERNS=(
    'twenty*'
    'genesis-block-theme'
    'Divi'
)

# List likely-custom WordPress themes in ./wp-content/themes
_rgignore_candidate_themes() {
    local themes_dir="./wp-content/themes"
    [[ ! -d "$themes_dir" ]] && return

    local name pattern skip
    for dir in "$themes_dir"/*(/N); do
        name="${dir:t}"
        skip=0
        for pattern in "${_RGIGNORE_DEFAULT_THEME_PATTERNS[@]}"; do
            if [[ "$name" == ${~pattern} ]]; then
                skip=1
                break
            fi
        done
        (( skip )) || echo "$name"
    done
}

# Render the {{THEMES}} block for the given theme names
_rgignore_render_themes() {
    local theme
    for theme in "$@"; do
        echo "!wp-content/themes/$theme/"
        echo "wp-content/themes/$theme/node_modules/"
        echo "wp-content/themes/$theme/vendor/"
    done
}

# Prompt for themes to keep. Uses fzf -m if available; falls back to a
# comma-separated read with detected candidates as the default.
_rgignore_read_themes() {
    local -a candidates selected
    candidates=("${(@f)$(_rgignore_candidate_themes)}")

    if (( ${#candidates[@]} )) && command -v fzf &>/dev/null; then
        selected=("${(@f)$(
            print -l -- "${candidates[@]}" |
                fzf -m --height 40% --border rounded \
                    --prompt="Themes to keep (TAB to multi-select) > " \
                    --header="ENTER with no selection = keep all detected"
        )}")
        if (( ${#selected[@]} == 0 )); then
            selected=("${candidates[@]}")
        fi
    else
        local default="${(j:,:)candidates}"
        local prompt_label="Themes to keep (comma-separated)"
        [[ -n "$default" ]] && prompt_label="$prompt_label [$default]"
        local response
        read "response?$prompt_label: "
        if [[ -z "$response" ]]; then
            selected=("${candidates[@]}")
        else
            selected=("${(@s:,:)response}")
        fi
    fi

    # Trim whitespace and emit one per line
    local t
    for t in "${selected[@]}"; do
        t="${t## }"; t="${t%% }"
        [[ -n "$t" ]] && echo "$t"
    done
}

# Expand {{PLACEHOLDER}} tokens in the template content read on stdin
_rgignore_expand() {
    local content="$1"

    if [[ "$content" == *'{{THEMES}}'* ]]; then
        local -a themes
        themes=("${(@f)$(_rgignore_read_themes)}")
        local rendered
        rendered="$(_rgignore_render_themes "${themes[@]}")"
        # Use zsh parameter substitution so we don't have to escape regex chars
        content="${content//\{\{THEMES\}\}/$rendered}"
    fi

    print -r -- "$content"
}

# Initialize .rgignore in the current directory from a template
rginit() {
    local template_dir="$_RGIGNORE_TEMPLATES"

    if [[ ! -d "$template_dir" ]]; then
        echo "Error: Templates directory not found: $template_dir"
        return 1
    fi

    local template_name="$1"

    if [[ -z "$template_name" ]]; then
        if ! command -v fzf &>/dev/null; then
            echo "Usage: rginit <template>"
            echo "Available templates:"
            for f in "$template_dir"/*.rgignore; do
                echo "  ${${f:t}%.rgignore}"
            done
            return 1
        fi

        template_name=$(
            for f in "$template_dir"/*.rgignore; do
                echo "${${f:t}%.rgignore}"
            done | fzf --height 40% --border rounded --prompt="Select rgignore template > " \
                       --preview "cat $template_dir/{}.rgignore"
        )

        [[ -z "$template_name" ]] && echo "Cancelled." && return 0
    fi

    local template_file="$template_dir/${template_name}.rgignore"

    if [[ ! -f "$template_file" ]]; then
        echo "Unknown template: $template_name"
        echo "Available templates:"
        for f in "$template_dir"/*.rgignore; do
            echo "  ${${f:t}%.rgignore}"
        done
        return 1
    fi

    if [[ -f ".rgignore" ]]; then
        echo ".rgignore already exists:"
        echo "--- .rgignore ---"
        cat .rgignore
        echo ""
        read "response?Overwrite? (y/N): "
        if [[ ! "$response" =~ ^[Yy]$ ]]; then
            echo "Cancelled."
            return 0
        fi
    fi

    local raw expanded
    raw="$(<"$template_file")"
    expanded="$(_rgignore_expand "$raw")"

    print -r -- "$expanded" > .rgignore
    echo "Created .rgignore from '$template_name' template"
}

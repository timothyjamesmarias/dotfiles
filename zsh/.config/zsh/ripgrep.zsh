# ripgrep.zsh - Ripgrep configuration and per-project ignore setup

export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

_RGIGNORE_TEMPLATES="$HOME/dotfiles/ripgrep/templates"

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

    if [[ -f ".rgignore" ]] || [[ -f ".fdignore" ]]; then
        echo "Ignore files already exist:"
        [[ -f ".rgignore" ]] && echo "--- .rgignore ---" && cat .rgignore
        [[ -f ".fdignore" ]] && echo "--- .fdignore ---" && cat .fdignore
        echo ""
        read "response?Overwrite? (y/N): "
        if [[ ! "$response" =~ ^[Yy]$ ]]; then
            echo "Cancelled."
            return 0
        fi
    fi

    cp "$template_file" .rgignore
    cp "$template_file" .fdignore
    echo "Created .rgignore and .fdignore from '$template_name' template"
}

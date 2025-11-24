# --- Utility functions ---

# Ripgrep configuration
export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

# AWS profile switcher
awsctx () {
	profile=${1:-noop}
	if [ "$profile" != "noop" ]; then
		export AWS_PROFILE=$profile
	else
		export AWS_PROFILE="$(aws configure list-profiles | fzf)"
		echo "Switched to profile \"$AWS_PROFILE\"."
	fi
}

# Refactoring aliases
alias rr='refactor-rename'
alias rri='refactor-rename --interactive'
alias rrp='refactor-rename --preview-only'

# File renaming
alias rf='rename-file'
alias rfn='rename-file --no-keep-path'

# --- Image optimization (TinyPNG-like compression) ---

# Optimize a single image (strips metadata, applies quality compression)
# Usage: imgopt image.png [quality 1-100, default 85]
imgopt() {
    local file="$1"
    local quality="${2:-85}"

    if [[ -z "$file" ]]; then
        echo "Usage: imgopt <image> [quality 1-100]"
        return 1
    fi

    if [[ ! -f "$file" ]]; then
        echo "Error: File not found: $file"
        return 1
    fi

    local ext="${file##*.}"
    local base="${file%.*}"
    local size_before=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")

    case "${ext:l}" in
        png)
            # PNG: strip metadata, reduce colors if possible, compress
            magick "$file" -strip -define png:compression-level=9 -quality "$quality" "$file"
            ;;
        jpg|jpeg)
            # JPEG: strip metadata, apply quality compression, use optimal sampling
            magick "$file" -strip -interlace Plane -sampling-factor 4:2:0 -quality "$quality" "$file"
            ;;
        webp)
            # WebP: strip metadata, apply quality compression
            magick "$file" -strip -quality "$quality" "$file"
            ;;
        gif)
            # GIF: optimize palette and strip metadata
            magick "$file" -strip -layers optimize "$file"
            ;;
        *)
            echo "Unsupported format: $ext (supported: png, jpg, jpeg, webp, gif)"
            return 1
            ;;
    esac

    local size_after=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")
    local saved=$((size_before - size_after))
    local percent=$((saved * 100 / size_before))

    echo "Optimized: $file"
    echo "  Before: $(numfmt --to=iec $size_before 2>/dev/null || echo "${size_before}B")"
    echo "  After:  $(numfmt --to=iec $size_after 2>/dev/null || echo "${size_after}B")"
    echo "  Saved:  ${percent}%"
}

# Optimize image to a new file (non-destructive)
# Usage: imgopt-copy image.png [output.png] [quality]
imgopt-copy() {
    local file="$1"
    local output="${2:-}"
    local quality="${3:-85}"

    if [[ -z "$file" ]]; then
        echo "Usage: imgopt-copy <image> [output] [quality 1-100]"
        return 1
    fi

    if [[ ! -f "$file" ]]; then
        echo "Error: File not found: $file"
        return 1
    fi

    local ext="${file##*.}"
    local base="${file%.*}"

    if [[ -z "$output" ]]; then
        output="${base}-optimized.${ext}"
    fi

    local size_before=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")

    case "${ext:l}" in
        png)
            magick "$file" -strip -define png:compression-level=9 -quality "$quality" "$output"
            ;;
        jpg|jpeg)
            magick "$file" -strip -interlace Plane -sampling-factor 4:2:0 -quality "$quality" "$output"
            ;;
        webp)
            magick "$file" -strip -quality "$quality" "$output"
            ;;
        gif)
            magick "$file" -strip -layers optimize "$output"
            ;;
        *)
            echo "Unsupported format: $ext"
            return 1
            ;;
    esac

    local size_after=$(stat -f%z "$output" 2>/dev/null || stat -c%s "$output")
    local saved=$((size_before - size_after))
    local percent=$((saved * 100 / size_before))

    echo "Created: $output"
    echo "  Original: $(numfmt --to=iec $size_before 2>/dev/null || echo "${size_before}B")"
    echo "  Optimized: $(numfmt --to=iec $size_after 2>/dev/null || echo "${size_after}B")"
    echo "  Saved: ${percent}%"
}

# Batch optimize all images in current directory
# Usage: imgopt-batch [quality] [--recursive]
imgopt-batch() {
    local quality="${1:-85}"
    local recursive=""

    if [[ "$1" == "--recursive" || "$2" == "--recursive" ]]; then
        recursive="-r"
        [[ "$1" == "--recursive" ]] && quality="${2:-85}"
    fi

    local total_before=0
    local total_after=0
    local count=0

    local find_cmd="find . -maxdepth 1"
    [[ -n "$recursive" ]] && find_cmd="find ."

    while IFS= read -r file; do
        [[ -z "$file" ]] && continue
        local size_before=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")
        total_before=$((total_before + size_before))

        imgopt "$file" "$quality" >/dev/null 2>&1

        local size_after=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")
        total_after=$((total_after + size_after))
        count=$((count + 1))
        echo "Optimized: $file"
    done < <(eval "$find_cmd" -type f \( -iname "*.png" -o -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.webp" -o -iname "*.gif" \))

    if [[ $count -gt 0 ]]; then
        local saved=$((total_before - total_after))
        local percent=$((saved * 100 / total_before))
        echo ""
        echo "Batch complete: $count files"
        echo "  Total before: $(numfmt --to=iec $total_before 2>/dev/null || echo "${total_before}B")"
        echo "  Total after:  $(numfmt --to=iec $total_after 2>/dev/null || echo "${total_after}B")"
        echo "  Total saved:  ${percent}%"
    else
        echo "No image files found"
    fi
}

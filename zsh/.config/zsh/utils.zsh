# --- Utility functions ---

# Claude Code launcher with unified menu
cc() {
    echo "Claude Code"
    echo "1) New conversation"
    echo "2) Resume previous"
    echo "3) Continue last"
    read "choice?> "
    case $choice in
        1) claude ;;
        2) claude --resume ;;
        3) claude --continue ;;
        *) echo "Invalid choice" ;;
    esac
}

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

# File renaming
alias rf='rename-file'
alias rfn='rename-file --no-keep-path'

# Project-specific format command
# Runs .format file in project root if it exists
fmt() {
	if [[ -f ".format" ]]; then
		echo "Running .format..."
		bash .format "$@"
	else
		echo "No .format file found in current directory"
		echo "Create a .format file with your project-specific format commands"
		return 1
	fi
}

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

    local find_cmd="find . -maxdepth 1"
    [[ -n "$recursive" ]] && find_cmd="find ."

    # Get total size before optimization
    local files=$(eval "$find_cmd" -type f \( -iname "*.png" -o -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.webp" -o -iname "*.gif" \) 2>/dev/null)

    if [[ -z "$files" ]]; then
        echo "No image files found"
        return
    fi

    local count=$(echo "$files" | wc -l | tr -d ' ')
    local total_before=0

    while IFS= read -r file; do
        [[ -z "$file" ]] && continue
        local size=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")
        total_before=$((total_before + size))
    done <<< "$files"

    echo "Optimizing $count images in parallel (quality: $quality)..."

    # Process images in parallel (4 at a time)
    echo "$files" | xargs -P 4 -I {} bash -c "imgopt '{}' '$quality' >/dev/null 2>&1 && echo 'Optimized: {}'"

    # Calculate total size after optimization
    local total_after=0
    while IFS= read -r file; do
        [[ -z "$file" ]] && continue
        local size=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file")
        total_after=$((total_after + size))
    done <<< "$files"

    local saved=$((total_before - total_after))
    local percent=$((saved * 100 / total_before))
    echo ""
    echo "Batch complete: $count files"
    echo "  Total before: $(numfmt --to=iec $total_before 2>/dev/null || echo "${total_before}B")"
    echo "  Total after:  $(numfmt --to=iec $total_after 2>/dev/null || echo "${total_after}B")"
    echo "  Total saved:  ${percent}%"
}

# Convert image to different format
# Usage: imgconvert <file> <extension> [width]
# Examples: imgconvert logo.svg png
#           imgconvert logo.svg png 2048
#           imgconvert photo.png jpg
imgconvert() {
    local file="$1"
    local ext="$2"
    local width="${3:-1024}"

    if [[ -z "$file" || -z "$ext" ]]; then
        echo "Usage: imgconvert <file> <extension> [width]"
        echo "  Convert image to different format"
        echo ""
        echo "Examples:"
        echo "  imgconvert logo.svg png        # Convert to PNG (1024px width)"
        echo "  imgconvert logo.svg png 2048   # Convert to PNG (2048px width)"
        echo "  imgconvert photo.png jpg       # Convert PNG to JPG"
        return 1
    fi

    if [[ ! -f "$file" ]]; then
        echo "Error: File not found: $file"
        return 1
    fi

    local base="${file%.*}"
    local input_ext="${file##*.}"
    local output="${base}.${ext}"

    # Check if output already exists
    if [[ -f "$output" ]]; then
        echo "Warning: $output already exists"
        read "?Overwrite? (y/N): " response
        if [[ ! "$response" =~ ^[Yy]$ ]]; then
            echo "Cancelled"
            return 0
        fi
    fi

    # Handle SVG conversion specially (needs rasterization settings)
    if [[ "${input_ext:l}" == "svg" ]]; then
        echo "Converting SVG: $file -> $output (width: ${width}px)"
        magick -density 300 -background none "$file" -resize "${width}x" "$output"
    else
        echo "Converting: $file -> $output"
        magick "$file" "$output"
    fi

    if [[ $? -eq 0 ]]; then
        local size=$(stat -f%z "$output" 2>/dev/null || stat -c%s "$output")
        echo "Created: $output ($(numfmt --to=iec $size 2>/dev/null || echo "${size}B"))"
    else
        echo "Error: Conversion failed"
        return 1
    fi
}

# --- Mermaid diagram utilities ---

# Compile mermaid diagram to PDF in /tmp
# Usage: mmd <file.mmd>
mmd() {
    local file="$1"

    if [[ -z "$file" ]]; then
        echo "Usage: mmd <file.mmd>"
        return 1
    fi

    if [[ ! -f "$file" ]]; then
        echo "Error: File not found: $file"
        return 1
    fi

    local base="${file%.*}"
    local basename="${base##*/}"
    local output="/tmp/${basename}.pdf"

    mmdc -i "$file" -o "$output"

    if [[ $? -eq 0 ]]; then
        echo "Compiled: $output"
    else
        echo "Error: Compilation failed"
        return 1
    fi
}

# Compile mermaid diagram and preview with macOS Preview
# Usage: mmdp <file.mmd>
mmdp() {
    local file="$1"

    if [[ -z "$file" ]]; then
        echo "Usage: mmdp <file.mmd>"
        return 1
    fi

    if [[ ! -f "$file" ]]; then
        echo "Error: File not found: $file"
        return 1
    fi

    local base="${file%.*}"
    local basename="${base##*/}"
    local output="/tmp/${basename}.pdf"

    mmdc -i "$file" -o "$output"

    if [[ $? -eq 0 ]]; then
        echo "Compiled: $output"
        open "$output"
    else
        echo "Error: Compilation failed"
        return 1
    fi
}

# Alias for combined compile and preview
alias mmdc='mmdp'

# --- Git aliases and Utilities ---

# Fuzzy switch to git branch
git_checkout() {
  local branch
  branch=$(git branch --all | grep -v HEAD | sed 's/.* //' | fzf) && git checkout "$branch"
}

# Fuzzy stage files
git_stage() {
  git status --short | fzf -m | awk '{print $2}' | xargs git add
}

# Fuzzy stage files - interactive
git_stage_patch() {
  local file
  file=$(git status --short | fzf | awk '{print $2}')
  [ -n "$file" ] && git add -p "$file"
}

# Fuzzy reset files
git_unstage() {
  git diff --cached --name-only | fzf -m | xargs git reset HEAD --
}

# Fuzzy commit from selected files
git_commit_fzf() {
  git_stage
  echo "Enter commit message:"
  read msg
  git commit -m "$msg"
}

# Fuzzy view commits with file preview
git_log_fzf() {
  local commit
  commit=$(git log --oneline --graph --decorate --color=always --all | \
    fzf --ansi \
        --no-sort \
        --reverse \
        --height=40% \
        --preview 'echo {} | grep -o "[a-f0-9]\{7,\}" | head -1 | xargs -I{} git show --color=always --stat {}' \
        --preview-window=right:60%)

  [ -n "$commit" ] && echo "$commit" | grep -o "[a-f0-9]\{7,\}" | head -1 | xargs git show
}

# Fuzzy select changed files and view diff (handles both staged and unstaged)
git_diff_fzf() {
  local files
  files=$(git status --short | fzf -m \
    --preview 'file=$(echo {} | cut -c4-);
      st=$(echo {} | cut -c1-2);
      if [[ "$st" =~ ^[^\ ?] ]]; then
        git diff --cached --color=always "$file";
      else
        git diff --color=always "$file";
      fi' \
    --preview-window=right:70%) || return

  if [[ -n "$files" ]]; then
    while IFS= read -r line; do
      # Extract first 2 chars as status, rest starting from char 4 as filename
      local st="${line:0:2}"
      local file="${line:3}"

      # Show staged diff if file is staged (first char is not space/?)
      if [[ "$st" =~ ^[^\ ?] ]]; then
        git diff --cached "$file"
      else
        git diff "$file"
      fi
    done <<< "$files"
  fi
}

# Fuzzy select staged files and view diff
git_diff_staged_fzf() {
  local files
  files=$(git diff --cached --name-only | fzf -m --preview 'git diff --cached --color=always {}' --preview-window=right:70%) || return

  if [[ -n "$files" ]]; then
    echo "$files" | xargs git diff --cached
  fi
}

# Fuzzy select commit and soft reset to it
git_reset_soft_fzf() {
  local commit
  commit=$(git log --oneline --graph --decorate --all | fzf --no-sort --reverse --preview 'echo {} | grep -o "[a-f0-9]\{7,\}" | head -1 | xargs git show --color=always' --preview-window=right:60%) || return

  if [[ -n "$commit" ]]; then
    local hash=$(echo "$commit" | grep -o "[a-f0-9]\{7,\}" | head -1)
    echo "Soft resetting to: $hash"
    git reset --soft "$hash"
  fi
}

# View file in GitHub with line numbers
view_in_github_fzf() {
  local root
  root=$(git rev-parse --show-toplevel) || return

  local file
  file=$(git ls-files | fzf --prompt="Select file to view on GitHub: ") || return

  local rel_path="$file"
  local branch=$(git rev-parse --abbrev-ref HEAD)
  local repo_url=$(gh repo view --json url -q .url)

  # Ask for optional line number (or range)
  echo -n "Enter start line (or leave empty): "
  read start_line
  echo -n "Enter end line (optional): "
  read end_line

  local line_part=""
  if [[ -n "$start_line" ]]; then
    line_part="#L$start_line"
    [[ -n "$end_line" && "$end_line" != "$start_line" ]] && line_part="#L$start_line-L$end_line"
  fi

  # Construct and open the URL
  local url="$repo_url/blob/$branch/$rel_path$line_part"
  echo "Opening: $url"
  if command -v open >/dev/null; then
    open "$url" # macOS
  else
    xdg-open "$url" # Linux
  fi
}

# Git worktree functions
# Create worktree for a branch
gwta() {
  if [ -z "$1" ]; then
    echo "Usage: gwta <branch> [directory]"
    return 1
  fi

  local branch="$1"
  local dir="${2:-../${PWD##*/}-${branch}}"

  git worktree add "$dir" "$branch"
  echo "Worktree created at: $dir"
}

# List worktrees
alias gwtl='git worktree list'

# Remove worktree
alias gwtr='git worktree remove'

# Fuzzy switch to worktree directory
gwts() {
  local worktree
  worktree=$(git worktree list | tail -n +2 | fzf --height=40% --reverse | awk '{print $1}')

  if [ -n "$worktree" ]; then
    cd "$worktree"
  fi
}

# Basic git aliases
alias gs='git status -sb'
alias gc='git commit -v'
alias gca='git commit -v --amend'
alias gl='git log --oneline --graph --decorate'
alias gp='git push'
alias gf='git pull --rebase'
alias gfa='git fetch && gf'
alias gcl='git clone'
alias gpo='git push origin'
alias gpu='git push -u origin HEAD'
alias gpf='git push --force-with-lease'
alias gco='git checkout'
alias gb='git checkout -b'
alias gd='git diff'
alias gds='git diff --staged'
alias gr='git restore'
alias grs='git restore --staged'
alias gcp='git cherry-pick'
alias gsw='git switch'
alias gpr='gh pr create --web'

# Fuzzy create fixup commit
gfix() {
  local commit
  # Try to get commits since upstream, fallback to last 20 commits
  commit=$(git log --oneline @{upstream}..HEAD 2>/dev/null || git log --oneline -n 20 | \
    fzf --preview 'echo {} | grep -o "[a-f0-9]\{7,\}" | head -1 | xargs git show --color=always --stat' \
        --preview-window=right:60% \
        --header='Select commit to create fixup for') || return

  local hash=$(echo "$commit" | grep -o "[a-f0-9]\{7,\}" | head -1)

  # Stage files interactively first
  echo "Staging files for fixup..."
  git_stage

  # Create fixup commit
  git commit --fixup="$hash"

  echo ""
  echo "✓ Created fixup for: $hash"
  echo "To apply: git rebase -i --autosquash $hash~1"
}

# Auto-rebase with autosquash
grba() {
  local base="${1:-@{upstream}}"
  if ! git rev-parse "$base" >/dev/null 2>&1; then
    echo "Error: Base '$base' not found"
    echo "Usage: grba [base-ref]"
    echo "Example: grba HEAD~5"
    return 1
  fi

  git rebase -i --autosquash "$base"
}

# Fuzzy git reflog navigation
grl() {
  local commit
  commit=$(git reflog --pretty=format:'%C(yellow)%h%Creset %C(cyan)%gd%Creset %gs %C(green)(%cr)%Creset' | \
    fzf --ansi \
        --preview 'echo {} | grep -o "[a-f0-9]\{7,\}" | head -1 | xargs git show --color=always --stat' \
        --preview-window=right:60% \
        --header='Select reflog entry (c=checkout, r=reset-soft, s=show)') || return

  local hash=$(echo "$commit" | grep -o "[a-f0-9]\{7,\}" | head -1)

  echo "Selected: $hash"
  echo -n "Action? (c)heckout, (r)eset-soft, (s)how [default=show]: "
  read action

  case "$action" in
    c) git checkout "$hash" ;;
    r) git reset --soft "$hash" ;;
    s|"") git show "$hash" ;;
    *) echo "Cancelled" ;;
  esac
}

# Git file history - browse commits that changed a file
gfh() {
  local file="${1}"

  if [ -z "$file" ]; then
    echo "Usage: gfh <file>"
    echo "  or: gfhf (to select file interactively)"
    return 1
  fi

  if [ ! -f "$file" ]; then
    echo "Error: File '$file' not found"
    return 1
  fi

  local commit
  commit=$(git log --oneline --follow --color=always -- "$file" | \
    fzf --ansi \
        --no-sort \
        --preview "echo {} | grep -o '[a-f0-9]\{7,\}' | head -1 | xargs -I{} git show --color=always {} -- $file" \
        --preview-window=right:70% \
        --header="History: $file (Enter=view full diff)")

  [ -n "$commit" ] && echo "$commit" | grep -o "[a-f0-9]\{7,\}" | head -1 | xargs -I{} git show {} -- "$file"
}

# Git file history with fuzzy file selection
gfhf() {
  local file
  file=$(git ls-files | fzf --preview 'bat --color=always --style=numbers {}' --preview-window=right:60%)

  [ -n "$file" ] && gfh "$file"
}

# Claude-assisted git commit
# Stages files interactively, then uses Claude to generate commit message
gclaude() {
  # Check if there are changes
  if git diff --cached --quiet && git diff --quiet; then
    echo "No changes to commit"
    return 1
  fi

  # Stage files if nothing staged
  if git diff --cached --quiet; then
    echo "No files staged. Staging files..."
    git_stage
  fi

  # Check again after staging
  if git diff --cached --quiet; then
    echo "No files staged, aborting"
    return 1
  fi

  # Get the diff for Claude
  local diff=$(git diff --cached)

  # Use Claude to generate commit message
  echo "Generating commit message with Claude..."
  local msg=$(echo "$diff" | claude -p "Generate a concise git commit message for this diff. Follow conventional commits format (feat/fix/refactor/docs/chore). Output ONLY the commit message, nothing else. Keep it under 72 chars for the first line." 2>/dev/null)

  if [ -z "$msg" ]; then
    echo "Failed to generate message, falling back to manual"
    git commit -v
    return
  fi

  echo ""
  echo "Proposed commit message:"
  echo "─────────────────────────"
  echo "$msg"
  echo "─────────────────────────"
  echo ""
  echo -n "(a)ccept, (e)dit, (r)egenerate, (c)ancel? "
  read action

  case "$action" in
    a) git commit -m "$msg" ;;
    e) git commit -v -m "$msg" ;;
    r) gclaude ;;
    *) echo "Cancelled" ;;
  esac
}

# Fuzzy git aliases
alias gcb="git_checkout"
alias ga="git_stage"
alias gap="git_stage_patch"
alias gu="git_unstage"
alias gcmsg="git_commit_fzf"
alias glog="git_log_fzf"
alias gcf="git log --oneline | fzf | cut -d ' ' -f1 | xargs git checkout"
alias ghviewf='view_in_github_fzf'
alias gdf='git_diff_fzf'
alias gdfs='git_diff_staged_fzf'
alias grsf='git_reset_soft_fzf'
alias gcl='gclaude'

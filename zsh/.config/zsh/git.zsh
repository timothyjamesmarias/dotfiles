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

# Fuzzy view commits
git_log_fzf() {
  git log --oneline --graph --decorate --all | fzf --no-sort --reverse --height=40%
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
alias gpf='git push --force-with-lease'
alias gco='git checkout'
alias gb='git branch -vv'
alias gd='git diff'
alias gds='git diff --staged'
alias gr='git restore'
alias grs='git restore --staged'
alias gcp='git cherry-pick'
alias gsw='git switch'
alias gpr='gh pr create --web'

# Fuzzy git aliases
alias gcb="git_checkout"
alias ga="git_stage"
alias gu="git_unstage"
alias gcmsg="git_commit_fzf"
alias glog="git_log_fzf"
alias gcf="git log --oneline | fzf | cut -d ' ' -f1 | xargs git checkout"
alias ghviewf='view_in_github_fzf'

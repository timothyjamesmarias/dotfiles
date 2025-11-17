# --- Utility functions ---

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

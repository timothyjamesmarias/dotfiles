# --- SSH Key Loading ---
# Keys are managed by the macOS system SSH agent (launchd).
# UseKeychain + AddKeysToAgent are set in ~/.ssh/config.d/defaults.conf
# so all processes (including GUI Emacs) share the same agent.

ssh-add --apple-use-keychain ~/.ssh/github 2>/dev/null
ssh-add --apple-use-keychain ~/.ssh/cablelabs 2>/dev/null

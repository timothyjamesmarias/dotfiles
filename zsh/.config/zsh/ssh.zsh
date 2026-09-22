# --- SSH Key Loading ---
# Keys are managed by the macOS system SSH agent (launchd) and load on demand:
# UseKeychain + AddKeysToAgent in ~/.ssh/config.d/defaults.conf let ssh pull
# the passphrase from the keychain, and config.d/github.conf names the
# non-default key file so ssh can find it without a prior ssh-add.
# No shell-startup ssh-add needed.

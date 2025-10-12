# --- GitHub SSH Agent Setup ---

# Start ssh-agent if not running
if [ -z "$SSH_AUTH_SOCK" ] || ! pgrep -u "$USER" ssh-agent >/dev/null; then
  eval "$(ssh-agent -s)" >/dev/null
fi

# Always add key if not already added
if ! ssh-add -l 2>/dev/null | grep -q "github"; then
  ssh-add ~/.ssh/github >/dev/null 2>&1
fi

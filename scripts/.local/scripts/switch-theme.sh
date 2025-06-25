#!/usr/bin/env bash
THEME="$1" # Accepts: main, moon, dawn

# Safety check
if [[ ! "$THEME" =~ ^(main|moon|dawn)$ ]]; then
  echo "Usage: switch-theme [main|moon|dawn]"
  exit 1
fi

# 🔁 NEOVIM (theme.lua symlink)
ln -sf "$HOME/.config/themes/rose-pine-$THEME/nvim.lua" "$HOME/.config/nvim/theme.lua"

# 🔁 ALACRITTY (swap import line)
sed -i'' -E "s|rose-pine-(main|moon|dawn)|rose-pine-${THEME}|" "$HOME/.config/alacritty/alacritty.yml"

# 🔁 TMUX (update variant + reload)
tmux set -g @rose_pine_variant "$THEME"
tmux source-file ~/.tmux.conf

# Optional: Notification
echo "Switched theme to: $THEME"

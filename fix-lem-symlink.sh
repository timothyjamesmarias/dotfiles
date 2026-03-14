#!/bin/bash
# Fix Lem symlink in PATH

echo "Fixing Lem symlink..."

# Remove old symlink if it exists
if [ -L /usr/local/bin/lem ]; then
    echo "Removing old symlink at /usr/local/bin/lem"
    sudo rm /usr/local/bin/lem
fi

# Create new symlink to correct location
echo "Creating new symlink: /usr/local/bin/lem -> ~/opt/lem/lem"
sudo ln -s "$HOME/opt/lem/lem" /usr/local/bin/lem

# Verify
if [ -L /usr/local/bin/lem ]; then
    echo "Symlink created successfully!"
    echo "Target: $(readlink /usr/local/bin/lem)"
    echo
    echo "You can now run 'lem' from anywhere."
else
    echo "Failed to create symlink."
    exit 1
fi
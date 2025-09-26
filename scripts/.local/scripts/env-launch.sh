#!/bin/bash
sleep 0.5

# Special boot entry point for Alacritty
if [[ "$1" == "--boot" ]]; then
  if ! tmux has-session -t default 2>/dev/null; then
    "$0" start
  fi

  exec tmux attach-session -t default
  exit
fi

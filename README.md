# Dotfiles

Personal dev environment, centered on Doom Emacs with a small layer of zsh utilities for terminal work.

## Layout

GNU Stow packages — each top-level dir maps into `$HOME`:

| Package        | Target                              |
| -------------- | ----------------------------------- |
| `alacritty/`   | `~/.alacritty.toml`                 |
| `claude/`      | `~/.claude/keybindings.json`        |
| `doom/`        | `~/.config/doom/`                   |
| `editorconfig/`| `~/.editorconfig`                   |
| `git/`         | `~/.gitconfig`, `~/.gitignore_global` |
| `ideavim/`     | `~/.ideavimrc` (JetBrains)          |
| `ripgrep/`     | `~/.ripgreprc`                      |
| `scripts/`     | `~/.local/scripts/`                 |
| `zsh/`         | `~/.zshrc`, `~/.config/zsh/`        |

Not stowed:
- `nixos/` — server config (see `nixos/README.md`)
- `templates/` — file templates consumed by `doom/.config/doom/modules/files.el` (see `templates/README.md`)
- `project-ideas/` — design notes

## Setup

```bash
./install-packages                                    # Brewfile via Homebrew
stow alacritty claude doom editorconfig git ideavim ripgrep scripts zsh
```

Doom is bootstrapped separately — see https://github.com/doomemacs/doomemacs.

## Where things live

- **Editor**: Doom Emacs. Config in `doom/.config/doom/`. Custom modules under `modules/` cover Claude Code, database (ejc-sql), Docker, file templates, framework detection, GitHub, macOS, Maizzle/Blade, notes, Rails navigation, and buffer cycling.
- **Shell**: zsh modules in `zsh/.config/zsh/`:
  - `prompt.zsh` — vi-mode indicator + git branch
  - `ssh.zsh` — auto-loads ssh keys
  - `keybindings.zsh` — vi mode, history nav, edit-command-line
  - `aliases.zsh` — basics + emacsclient aliases (`e`, `et`, `ec`)
  - `git.zsh` — fzf-driven git workflows (`gcb`, `ga`, `gcmsg`, `glog`, `gdf`, `grl`, worktrees, `gclaude`)
  - `utils.zsh` — `imgopt`, `imgconvert`
  - `ripgrep.zsh` — `rginit` for per-project `.rgignore` from `ripgrep/templates/`
- **Git**: editor is `emacsclient --socket-name=doom`, pager is delta, conflict style is diff3.

## Philosophy

- Emacs for most work; zsh stays small and composable
- Native configs, no abstraction layers
- Stow for symlinking, asdf/sdkman for language versions

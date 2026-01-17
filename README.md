# Dotfiles

Personal development environment configuration.

## Setup

### 1. Install Packages

```bash
./install-packages
```

This installs all required packages declaratively:
- **macOS**: Uses `Brewfile` (Homebrew bundle)
- **Linux**: Uses `fedora-packages.txt` (dnf)

### 2. Symlink Dotfiles (via Stow)

```bash
cd ~/dotfiles
stow alacritty ctags git nvim scripts tmux zsh
```

### 3. Install Language Toolchains

Using asdf/sdkman (managed separately):

```bash
# Install sdkman (for JVM languages)
curl -s "https://get.sdkman.io" | bash

# asdf is already installed via Brewfile/fedora-packages.txt
# Add language plugins as needed:
asdf plugin add nodejs
asdf plugin add ruby
# etc.
```

## Managing Packages

### macOS (Homebrew)

```bash
# Install/update packages
brew bundle --file=~/dotfiles/Brewfile

# Remove packages not in Brewfile
brew bundle cleanup --file=~/dotfiles/Brewfile --force

# Add new package
echo 'brew "package-name"' >> ~/dotfiles/Brewfile
brew bundle --file=~/dotfiles/Brewfile
```

### Linux (Fedora)

```bash
# Add new package
echo 'package-name' >> ~/dotfiles/fedora-packages.txt

# Install
./install-packages

# Update all packages
sudo dnf upgrade -y
```

## Philosophy

- **Brewfile/package lists**: Declarative package management (version-controlled)
- **Stow**: Dotfile symlinking
- **asdf/sdkman**: Language version management (per-project)
- **Native configs**: Shell, Emacs, Neovim (no abstraction layers)
- **Simple**: Use platform-native tools, no complex meta-layers

# Lem Editor Setup Complete

## Installation Summary

Lem has been successfully installed and configured on your macOS system.

### Locations

- **Application**: `~/opt/lem/lem` (118MB)
- **macOS App Bundle**: `~/Applications/Lem.app`
- **Configuration**: `~/dotfiles/lem/`
- **Executable in PATH**: `~/.local/bin/lem` → `~/opt/lem/lem`
- **Config symlink**: `~/.config/lem/init.lisp` → `~/dotfiles/lem/init.lisp`

### Running Lem

**From the command line:**
```bash
lem                    # Launch Lem
lem myfile.txt        # Open a specific file
```

**As a macOS application:**
- Press `Cmd+Space` and type "Lem" (Spotlight)
- Double-click `Lem.app` in `~/Applications`
- Add to Dock by dragging from `~/Applications`
- Right-click files → Open With → Lem

### Frontend Options

Lem supports multiple frontends:
- **WebView** (default): `lem` or `lem --frontend webview`
- **Terminal**: Build with `cd ~/opt/lem && make ncurses`
- **SDL2 GUI**: Build with `cd ~/opt/lem && make sdl2`

Note: WebView on macOS has limitations. The ncurses (terminal) frontend is most stable.

### Configuration

Your Lem configuration is stored in `~/dotfiles/lem/init.lisp`.

To customize:
```bash
# Edit the configuration
vim ~/dotfiles/lem/init.lisp

# Or
code ~/dotfiles/lem/init.lisp
```

Changes take effect on next Lem startup.

### Installing on Another Machine

Use the install script:
```bash
bash ~/dotfiles/install-lem.sh
```

This will:
1. Install dependencies (SBCL, GTK4, qlot)
2. Clone Lem to `~/opt/lem`
3. Build the webview version
4. Set up configuration in `~/dotfiles/lem`
5. Create symlinks automatically

### Updating Lem

```bash
cd ~/opt/lem
git pull
make webview  # or ncurses, or sdl2
```

### Troubleshooting

**"lem: command not found"**
- Check: `ls -la ~/.local/bin/lem`
- If missing, run: `ln -s ~/opt/lem/lem ~/.local/bin/lem`
- Or run directly: `~/opt/lem/lem`

**Build errors**
- Clear cache: `rm -rf ~/opt/lem/.qlot`
- Retry: `cd ~/opt/lem && make webview`

**Configuration not loading**
- Check symlink: `ls -la ~/.config/lem/init.lisp`
- Should point to: `~/dotfiles/lem/init.lisp`

### Documentation

- **Installation Notes**: `~/dotfiles/lem-installation-notes.md`
- **Configuration Docs**: `~/dotfiles/lem/README.md`
- **Official Docs**: https://lem-project.github.io/

## macOS-Specific Notes

- WebKitGTK is not natively available on macOS
- WebView frontend may have limited functionality
- Recommend using ncurses (terminal) or SDL2 frontends for best experience

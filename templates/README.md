# File Templates System

A Unix-style, IDE-like file creation system with modern ergonomics.

## Overview

This system provides three complementary ways to create files:

1. **Quick & Simple (`cf`)**: Fast file creation without templates (from zsh)
2. **Template-based (`new-file`)**: Smart, IDE-like file creation with templates
3. **Neovim**: Automatic default templates when opening new files in Vim

### When to Use Which Tool

```
cf()          → "I need a file NOW, any type, no structure needed"
new-file      → "I'm creating a class/component/interface with proper structure"
nvim new.vue  → "Opening a new file in vim, auto-load simple template"
```

**Examples:**
```bash
# Quick scratch file or config
cf                           # Fast: pick dir → type name → edit

# Structured component with template
new-file                     # Interactive: dir → language → kind → name → preview

# Ad-hoc file in vim
nvim src/utils/helper.ts     # Auto-loads TypeScript default template
```

## GNU Stow Setup

This system is designed to work with GNU Stow for dotfile management:

**Directory Structure:**
```
~/dotfiles/
  scripts/.local/scripts/new-file  → stows to ~/.local/scripts/new-file
  nvim/.config/nvim/               → stows to ~/.config/nvim/
  templates/                       → stays in ~/dotfiles/ (not stowed)
```

**How it works:**
- Both the script and Neovim config automatically resolve symlinks to find the templates
- Templates live in `~/dotfiles/templates/` and are accessed by resolving the symlinked paths
- No manual configuration needed - it just works!

## Quick Start

### Interactive Mode

```bash
new-file
```

This launches an interactive wizard that walks you through:
1. **Where** - Select target directory (current or browse)
2. **Language** - Choose from available languages (PHP, TypeScript, Vue, etc.)
3. **Kind** - Select template type (class, interface, component, etc.)
4. **Name** - Enter file name (supports nested paths: `auth/forms/LoginForm.vue`)
5. **Confirm** - Preview and confirm before creation

### Quick Mode

Create files directly:

```bash
new-file src/components/Button.vue
new-file --lang php --kind class User
```

### Neovim Mode

Just open a new file in Vim:

```bash
nvim NewComponent.vue    # Auto-loads Vue template
nvim script.sh           # Auto-loads shell template with shebang
```

### Quick File Creation with `cf()`

For times when you don't need templates, use the `cf()` zsh function:

```bash
cf
# Select directory with fzf
# Enter: auth/LoginForm.vue
# Creates nested dirs and opens in nvim immediately
```

**`cf()` vs `new-file`:**
- `cf()` - No templates, fastest path to editing
- `new-file` - Templates, structure, previews, smart placeholders

## Features

- **Smart placeholders**: Automatically replaces `{{CLASS_NAME}}`, `{{NAMESPACE}}`, `{{COMPONENT_NAME}}`, etc.
- **Case conversion**: Converts names to PascalCase, kebab-case, snake_case as needed
- **Nested path support**: Use slashes in file names to create directory structures
- **Directory creation**: Creates parent directories if they don't exist
- **Preview**: See file content before creation
- **Git-aware**: Works seamlessly in git repositories
- **Extensible**: Easy to add new languages and templates

## Available Templates

### PHP
- **class** - PHP class with namespace
- **interface** - PHP interface with namespace
- **default** - Basic PHP file

### TypeScript
- **class** - TypeScript class
- **interface** - TypeScript interface
- **component** - Angular component
- **default** - TypeScript module

### JavaScript
- **class** - ES6 class
- **module** - JavaScript module
- **default** - JavaScript file

### Vue
- **component** - Vue 3 component with script setup
- **default** - Basic Vue component

### Python
- **class** - Python class
- **default** - Python script with main function

### HTML
- **default** - HTML5 boilerplate

### Shell
- **default** - Bash script with strict mode

## Placeholders

Templates support these placeholders:

- `{{CLASS_NAME}}` - PascalCase class name (e.g., `MyButton`)
- `{{INTERFACE_NAME}}` - PascalCase interface name
- `{{MODULE_NAME}}` - Original file name without extension
- `{{COMPONENT_NAME}}` - kebab-case component name (e.g., `my-button`)
- `{{FILE_NAME}}` - File name without extension
- `{{NAMESPACE}}` - Derived from directory path (PHP-style)
- `{{TITLE}}` - Human-readable title
- `{{SELECTOR}}` - kebab-case selector for Angular

## Adding New Templates

1. **Create template file**:
   ```bash
   mkdir -p templates/rust
   cat > templates/rust/struct.rs << 'EOF'
   pub struct {{CLASS_NAME}} {
       // fields
   }
   EOF
   ```

2. **Register in config** (`templates.conf`):
   ```
   rust|rs|struct|rust/struct.rs|Rust Struct
   rust|rs|default|rust/default.rs|Rust File
   ```

3. **Add to Neovim** (optional, `nvim/.config/nvim/lua/tim/templates.lua`):
   ```lua
   { pattern = "*.rs", template = "rust/default.rs" },
   ```

## Configuration

### Template Config Format

`templates/templates.conf`:
```
language|extension|kind|template_path|description
```

Example:
```
php|php|class|php/class.php|PHP Class
```

### Neovim Config

Located at `nvim/.config/nvim/lua/tim/templates.lua`

Uses only "default" templates for simple, ad-hoc file creation. For complex types (classes, interfaces), use the `new-file` command.

## Usage Examples

### Create a Vue component interactively
```bash
new-file
# Select: src/components → vue → component → Button.vue
```

### Create a PHP class with options
```bash
new-file --lang php --kind class --dir src/Services --name UserService
```

### Quick create from path
```bash
new-file src/components/Header.vue
# Auto-detects Vue, uses component template
```

### Neovim quick create
```bash
nvim src/utils/helper.ts  # Auto-loads TypeScript default template
```

## Philosophy

- **Unix-style**: Composable, scriptable, works with other tools
- **Modern UX**: fzf-powered menus, previews, smart defaults
- **Non-intrusive**: Neovim stays simple, complexity lives in CLI tools
- **Extensible**: Easy to add languages without modifying core logic

## Integration

### Tmux
Bind `new-file` to a tmux key for quick access:
```bash
bind-key C-n run-shell "tmux split-window -h new-file"
```

### Shell Alias
```bash
alias nf='new-file'
```

### Custom Scripts
The `new-file` script can be called from other scripts:
```bash
new-file --lang typescript --kind class --name MyClass --no-confirm
```

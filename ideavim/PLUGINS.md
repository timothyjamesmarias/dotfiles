# IntelliJ IDEA Plugin Recommendations

Curated list of plugins to enhance your IdeaVim workflow and match your Unix/tmux paradigms.

---

## Essential Plugins (Install These First)

### 1. **Which-Key** ⭐
**Install:** Settings → Plugins → Marketplace → Search "Which-Key"
**Purpose:** Displays available keybindings in a popup as you type (like your `cmd-finder`)

**Configuration in `.ideavimrc`:**
```vim
" Enable which-key extension
set which-key

" Show popup after 500ms
set notimeout

" Optional: Show vim's default mappings too
let g:WhichKey_ShowVimActions = "true"

" Optional: Define custom descriptions
let g:WhichKey_DefaultDelay = 500
```

**Why you need this:**
- Your `cmd-finder` script is brilliant for discovering shell/vim commands
- Which-Key gives you the same discoverability IN IntelliJ
- As you type `<leader>f`, it shows all `<leader>f*` mappings
- Perfect for learning your new mappings without leaving the editor

---

### 2. **IdeaVim-EasyMotion** + **AceJump** ⭐
**Install:** Both plugins from Marketplace

**Configuration in `.ideavimrc`:**
```vim
" Enable easymotion
Plug 'easymotion/vim-easymotion'

" Map to your preferred key (like your fzf fuzzy finding)
map <leader>j <Plug>(easymotion-s)
map <leader>J <Plug>(easymotion-f)
```

**Usage:**
- `<leader>j` then type a character → jump anywhere on screen
- `<leader>J` then type character → jump forward
- Like having `fzf` for cursor movement within the file

**Why you need this:**
- Matches your "fuzzy everything" paradigm
- Jump to any visible text without counting lines or using motions
- Way faster than `hjkl` for long-distance movement

---

### 3. **String Manipulation** ⭐
**Install:** Settings → Plugins → Marketplace → Search "String Manipulation"

**Mappings (already added to your `.ideavimrc`):**
```vim
nmap <leader>sm <Action>(StringManipulation.PopupChoiceAction)
nmap <leader>st <Action>(osmedile.intellij.stringmanip.ToggleCaseAction)
nmap <leader>ss <Action>(osmedile.intellij.stringmanip.SortAction)
```

**Features:**
- Toggle case: camelCase ↔ snake_case ↔ kebab-case ↔ PascalCase
- Sort lines, align to columns
- Escape/unescape (Java, JSON, XML, HTML, SQL)
- Increment/decrement numbers
- Filter/grep lines

**Why you need this:**
- Replaces your shell text manipulation (sed/awk/tr)
- Works on selections like vim visual mode
- Perfect for refactoring API naming conventions

---

## Workflow Enhancement Plugins

### 4. **GitToolBox**
**Install:** Marketplace → Search "GitToolBox"

**Features:**
- Inline git blame (like your `<leader>gb` but always visible)
- Auto-fetch status
- Branch cleanup tools
- Commit message validation

**Why you need this:**
- Complements your git.zsh functions
- Visual git status in editor (staged/unstaged indicators)
- Inline blame shows who changed each line

---

### 5. **Key Promoter X**
**Install:** Marketplace → Search "Key Promoter X"

**Features:**
- Shows keyboard shortcuts when you use mouse/menus
- Tracks which actions you use most without shortcuts
- Suggests creating shortcuts for frequent actions

**Why you need this:**
- Helps you discover shortcuts you don't know
- Reinforces keyboard-first workflow (like your tmux bindings)
- Builds muscle memory for IntelliJ actions

---

### 6. **Rainbow Brackets**
**Install:** Marketplace → Search "Rainbow Brackets"

**Features:**
- Colors matching brackets/parens with rainbow colors
- Makes nested code easier to scan

**Why you need this:**
- Better visual feedback (matches your colored shell output)
- Especially helpful in deeply nested code (Kotlin DSLs, JSON)

---

### 7. **Grep Console**
**Install:** Marketplace → Search "Grep Console"

**Features:**
- Color-code console output by patterns (ERROR=red, WARN=yellow)
- Filter console output with regex
- Tail/fold log sections

**Why you need this:**
- Makes IntelliJ's console feel like `tail -f` with color
- Perfect for Spring Boot logs, test output
- Matches your shell's colored output aesthetic

---

## Advanced/Optional Plugins

### 8. **HTTP Client** (Built-in, but underused)
**Already installed!** Just create `.http` files

**Example workflow:**
1. Create `api-requests.http` in your project
2. Write HTTP requests:
   ```http
   ### Get user
   GET https://api.example.com/users/1
   Authorization: Bearer {{token}}

   ### Create user
   POST https://api.example.com/users
   Content-Type: application/json

   {
     "name": "John Doe"
   }
   ```
3. Run with `<leader>He` (mapped in your config)

**Why you need this:**
- Replaces Postman/curl for API testing
- Version control your API requests
- Use variables and environments

---

### 9. **Database Navigator** or **Database Tools** (Built-in)
**Already included in IntelliJ Ultimate!**

**Your mappings:**
```vim
nmap <leader>DD <Action>(ActivateDatabaseToolWindow)
nmap <leader>Dq <Action>(Console.Jdbc.Execute)
nmap <leader>Dn <Action>(Console.Jdbc.NewConsole)
```

**Features:**
- SQL autocomplete with schema awareness
- Execute queries inline
- Visual query builder
- Database diff/sync

**Why you need this:**
- Keep SQL scripts in version control
- Execute queries without leaving editor
- Schema introspection

---

### 10. **Presentation Assistant**
**Install:** Marketplace → Search "Presentation Assistant"

**Features:**
- Shows what keyboard shortcuts you're using (for pair programming/streaming)
- Displays action names when you trigger them

**Why you need this:**
- Great for learning new shortcuts
- Useful when pairing/teaching
- Helps identify which action was just triggered

---

## IdeaVim Plugin Extensions (Add to `.ideavimrc`)

IntelliJ has special IdeaVim extensions you can enable with `Plug` syntax:

```vim
" Commentary - toggle comments with gc
Plug 'tpope/vim-commentary'

" Surround - add/change/delete surrounding quotes, brackets
Plug 'tpope/vim-surround'

" Multiple cursors
Plug 'terryma/vim-multiple-cursors'

" Argument text objects (function arguments)
Plug 'vim-scripts/argtextobj.vim'

" Entire buffer text object (vae, dae)
Plug 'kana/vim-textobj-entire'

" Exchange operator (swap text)
Plug 'tommcdo/vim-exchange'
```

Already in your config:
```vim
set commentary
set surround
```

---

## Plugin Installation Workflow

### Method 1: Via UI
1. `Cmd+,` (Settings)
2. Plugins → Marketplace
3. Search plugin name
4. Click "Install"
5. Restart IntelliJ

### Method 2: Via Settings Sync (Recommended)
Enable Settings Sync to sync plugins across machines:
1. File → Manage IDE Settings → Settings Sync
2. Sign in with JetBrains account
3. Select what to sync (including plugins)

---

## Recommended Plugin Load Order

Install in this order to build up your workflow:

1. **Which-Key** - Learn your mappings
2. **IdeaVim-EasyMotion + AceJump** - Navigate faster
3. **String Manipulation** - Text operations
4. **Key Promoter X** - Discover more shortcuts
5. **Rainbow Brackets** - Visual clarity
6. **GitToolBox** - Git integration
7. **Grep Console** - Better logs
8. **Presentation Assistant** - Learning aid

---

## Testing Your Setup

After installing plugins, test these workflows:

### Discoverability Test
1. Press `<leader>` and wait 500ms
2. Which-Key popup should show all `<leader>*` mappings
3. Press `f` → see all `<leader>f*` file operations
4. Press `g` → see all `<leader>g*` git operations

### Navigation Test
1. Open a large file
2. Press `<leader>j` (easymotion)
3. Type a character that appears multiple times
4. Labels should appear → type label to jump

### Text Manipulation Test
1. Select a variable name in camelCase
2. Press `<leader>sm`
3. Choose transformation (to snake_case, etc.)
4. Verify it changed

### Git Integration Test
1. Open a file with git history
2. Press `<leader>gb` → see inline blame
3. Press `<leader>gh` → see file history
4. Select commit → diff view

---

## Alternative Plugin Bundles

If you want a pre-configured setup:

### **Intellimacs**
GitHub: marcoieni/intellimacs
Spacemacs-like key bindings for IntelliJ (similar to your tmux modal approach)

**Pros:**
- Pre-configured modal bindings
- Mnemonic key groupings (like your organized `<leader>` groups)
- Active community

**Cons:**
- Might conflict with your existing config
- Learning curve for Spacemacs conventions

**Recommendation:** Study their `.ideavimrc` for ideas, but keep your custom config

---

## Discoverability: Building Your Own `cmd-finder` for IntelliJ

Your `cmd-finder` script is genius. Here's how to replicate it in IntelliJ:

### Option 1: Use Which-Key (Recommended)
- Already shows mappings as you type
- No extra work needed

### Option 2: Create a Custom Action List Script
Create `~/bin/idea-cmd-finder`:

```bash
#!/usr/bin/env bash
# Fuzzy search through IdeaVim mappings and IntelliJ actions

CACHE_FILE="$HOME/.cache/idea-cmd-finder"
IDEAVIMRC="$HOME/.ideavimrc"

# Extract mappings from .ideavimrc
extract_mappings() {
  grep -E '^[vn]?map ' "$IDEAVIMRC" | \
    perl -ne 'if (/^(\w*)map\s+(\S+)\s+(.+)/) {
      $mode = $1 || "normal";
      $key = $2;
      $action = $3;
      $action =~ s/<Action>\(([^)]+)\)/$1/;
      print "$mode\t$key\t$action\n"
    }'
}

# Get IntelliJ actions
extract_actions() {
  # Would need to query IntelliJ's action registry
  # For now, parse from common actions
  echo -e "action\tGotoFile\tFind file by name"
  echo -e "action\tGotoClass\tFind class by name"
  # ... add more
}

# Build cache
{
  extract_mappings
  extract_actions
} > "$CACHE_FILE"

# Fuzzy find
cat "$CACHE_FILE" | \
  column -t -s $'\t' | \
  fzf --height=80% \
      --layout=reverse \
      --preview 'echo {}' \
      --header="IntelliJ Commands (Ctrl-Y to copy)"
```

### Option 3: Use IntelliJ's Built-in Search
- Double-tap `Shift` → SearchEverywhere (already has fuzzy find)
- Switch to "Actions" tab → all IntelliJ actions
- Your mapping: `<leader><leader>a` → `GotoAction`

**Recommendation:** Use `<leader><leader>a` for action search + Which-Key for mapping discovery

---

## Maintenance & Updating

### Keep Plugins Updated
- IntelliJ → Help → Check for Updates
- Auto-updates recommended unless you need stability

### Backup Your Config
Your dotfiles already track `.ideavimrc` ✓

Also backup:
- IntelliJ settings: File → Manage IDE Settings → Export Settings
- Or use Settings Sync for automatic backup

### Debugging Plugin Conflicts
If something breaks:
1. Help → Find Action → "Registry"
2. Search for plugin name
3. Disable temporarily
4. Or check `idea.log`: Help → Show Log in Finder

---

## Summary: Essential Setup

For your workflow, prioritize these 3:

1. ⭐ **Which-Key** - Discoverability (like cmd-finder)
2. ⭐ **IdeaVim-EasyMotion + AceJump** - Fuzzy navigation (like fzf)
3. ⭐ **String Manipulation** - Text operations (like sed/awk)

These give you 80% of the value and match your paradigms perfectly.

Install the rest as needed when you hit friction in specific workflows.

---

## Questions or Issues?

If a plugin isn't working:
1. Check plugin is enabled: Settings → Plugins
2. Verify `.ideavimrc` syntax: `:source ~/.ideavimrc`
3. Check IntelliJ version compatibility
4. Look at plugin's GitHub issues
5. Run `:actionlist <plugin-name>` to see available actions

Happy coding! 🚀

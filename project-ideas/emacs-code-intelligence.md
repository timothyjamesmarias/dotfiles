# Emacs Code Intelligence — Design Notes

Framework-aware navigation handlers and a semantic index backend for Doom Emacs, extending its existing `+lookup` dispatch system rather than replacing it.

## Motivation

LSP quality varies wildly across languages:

- **Good:** intelliphense (PHP), gopls, rust-analyzer, clangd
- **Mediocre:** ts-server, css-ls
- **Bad:** ruby-lsp, solargraph

Even where LSP is good, `lsp-mode` and `eglot` impose cost on Emacs' single main thread — JSON parsing, response handling, and chatty features (inlay hints, semantic tokens, document highlighting) all compete for the same event loop.

Meanwhile, the navigation that actually matters day-to-day — convention-based jumps in framework-heavy projects (Rails partials, SCSS from templates, Spring route handlers) — isn't covered by LSP, tags, or grep at all.

## Why Doom's `+lookup` Is the Right Foundation

Doom already unifies LSP, ctags/xref, dumb-jump, and ripgrep into a single dispatch chain via its `+lookup` module. The architecture:

- **Hook-based dispatch.** Each query type (`definition`, `references`, `implementations`, etc.) has an ordered list of backend functions. Each is tried in sequence; first one that succeeds wins.
- **Fallback chain.** Default order: mode-specific handlers (LSP if active) → xref backend (etags) → dumb-jump → ripgrep → evil text search.
- **`set-lookup-handlers!`** registers per-mode backends that get prepended (buffer-local) to the chain, running before the global defaults.
- **Per-language configurability** is built in — different major/minor modes get different handlers.

This is already the multi-backend dispatcher architecture. We don't need to build it — we need to **add smarter backends to it**.

## What's Missing from Doom's Default Setup

### 1. Framework-Aware Navigation Handlers

Nobody has written the handlers for convention-based jumps:

- **Rails:** `render partial: 'foo'` → `app/views/_foo.html.slim`
- **Slim/ERB → SCSS:** template class `.foo` → SCSS rule `.foo` (string matching with path conventions)
- **Blade components:** `<x-alert>` → `App\View\Components\Alert.php`
- **Spring Boot:** `@GetMapping("/foo")` → route handler (annotation scanning)
- **Gradle beans:** shell out to `gradle` for dependency/bean mappings, cache results

Each of these is a small elisp function registered via `set-lookup-handlers!` on the relevant major mode. They aren't generic — they encode specific framework conventions. That's a feature, not a limitation.

### 2. Semantic Index Backend (for weak-LSP languages)

For languages where LSP is bad (Ruby) or absent (templates), a SCIP-backed semantic index provides type-resolved cross-file navigation that grep can't:

- Cross-file references with correct symbol identity (not just string matching)
- Unused symbol detection
- Implementation/caller graphs

**Dora** already provides SCIP → SQLite indexing with a query interface and MCP server mode. Evaluate it before building anything custom. If it works, register a lookup handler that shells out to Dora's CLI or queries its SQLite directly.

### 3. Per-Project Framework Detection

Doom doesn't auto-detect "this is a Rails project, activate Rails navigation rules." Options:

- **`.dir-locals.el`** — declare project type, enable the right minor modes / handlers
- **Auto-detection minor mode** — check for `Gemfile` + `config/routes.rb` → Rails; `build.gradle` → Spring Boot; `composer.json` + `artisan` → Laravel
- Minor mode activation triggers `set-lookup-handlers!` registration for that framework

## Architecture

### Delivery

A Doom module (e.g., `:tools framework-nav`) or a set of config additions in the user's `config.el`. Not a standalone Emacs package — this lives inside Doom's ecosystem.

### Handler Contract

Each framework handler follows Doom's existing convention:

```elisp
(defun +rails-partial-lookup-handler (identifier)
  "Jump to a Rails partial from a render call."
  ;; Parse the partial reference, resolve to file path, open it.
  ;; Return non-nil on success, nil to fall through to next handler.
  ...)

(set-lookup-handlers! 'slim-mode
  :definition #'+rails-partial-lookup-handler)
```

For the SCSS-from-template case, the handler might be registered on `slim-mode` and `web-mode` for the `:definition` lookup type, so `gd` on a CSS class in a template jumps to the SCSS rule.

### Async Discipline

All handlers that shell out to external tools (Dora, gradle, ripgrep) must be async:

- Use `make-process` or `async-shell-command`, not `shell-command-to-string`
- Return `'deferred` from the handler to tell Doom's dispatch not to try the next backend
- Deliver results via callback

For handlers that do pure elisp string/path resolution (Rails partial lookup, Blade component resolution), synchronous is fine — these are just string manipulation, no I/O.

### Semantic Index Integration

```
┌─────────────┐     ┌───────────────┐     ┌──────────────┐
│ SCIP indexer │ ──▶ │ Dora / scq    │ ──▶ │ SQLite index │
│ (per-lang)   │     │ (normalizer)  │     │ (.dora.db)   │
└─────────────┘     └───────────────┘     └──────┬───────┘
                                                  │
                                    ┌─────────────┼──────────────┐
                                    ▼             ▼              ▼
                              Emacs handler   CLI queries   MCP server
                              (gd, gD, etc.)  (shell/pipe)  (agents)
```

Emacs handler queries SQLite directly (via `sqlite.el` in Emacs 29+ or shelling out) for definition/reference lookups. Falls through to dumb-jump/ripgrep if the index doesn't have an answer.

## Agent Integration

Separate from the Emacs layer, the semantic index serves AI agents:

- **MCP server mode** — Dora already supports this; Claude queries the index directly
- **JSON output** — structured symbol data, reference graphs, impact analysis
- **Bulk queries** — "all symbols in this module and their relationships"

Agents benefit more from semantic data than human editors do — they can't intuitively disambiguate grep results the way a developer can.

## Freshness Model

- **LSP:** real-time (manages its own state)
- **ctags:** regenerated on save
- **ripgrep / dumb-jump:** always current (reads files directly)
- **tree-sitter:** incremental per-edit (Emacs native)
- **Semantic index:** updated on save via filesystem watcher. Stale by seconds. Acceptable for navigation.
- **Framework handlers:** string/path resolution is instant. Framework CLI results (gradle) are cached, refreshed on demand.

For live-edit accuracy (hover, diagnostics, mid-keystroke type info), LSP remains the right tool. This complements LSP, it doesn't replace it.

## What This Is Not

- Not a new Emacs package or navigation framework — it extends Doom's existing `+lookup`
- Not an LSP replacement for languages where LSP works well
- Not a new language analyzer or type checker
- Not a general-purpose FOSS project — personal tool built for specific workflows
- Not keystroke-level semantic accuracy — that requires an in-process compiler

## Phased Work

**Phase 0 — Agent path (lowest effort, highest immediate value)**
- Set up Dora on a real project, evaluate its MCP server with Claude
- Determine if the semantic index is useful enough for agent workflows to justify maintaining

**Phase 1 — Framework handlers (highest daily-use value)**
- Rails: partial resolution, route jumping, SCSS-from-template
- Register via `set-lookup-handlers!` on relevant major modes
- Auto-detection minor mode for project type

**Phase 2 — Semantic index Emacs backend**
- Lookup handler that queries Dora's SQLite for definition/references
- Registered as high-priority handler for weak-LSP languages (Ruby, etc.)
- Falls through to dumb-jump/ripgrep when index misses

**Phase 3 — Additional framework rules**
- Spring Boot / Gradle
- Laravel / Blade
- Vue SFC cross-boundary navigation

Each phase is independent. Phase 0 validates the index. Phase 1 delivers immediate editing value with zero infrastructure. Phase 2 connects them.

## Open Questions

- **Dora evaluation.** Does Dora's MCP server + SQLite schema actually cover the agent use case well enough? Try it before building anything.
- **Which Rails jumps first?** Partial resolution is probably the most common. SCSS-from-Slim is the most annoying to do manually.
- **`sqlite.el` vs. shell-out for Emacs queries?** Emacs 29+ has native SQLite support. Faster than shelling out, but ties the handler to Emacs 29+.
- **How much of this belongs in Doom config vs. a proper module?** If it stays personal, `config.el` is fine. If it grows, a module gives better structure.

## Relationship to plan.md

The original `plan.md` describes a broader vision: a Unix-native semantic code intelligence layer with SCIP indexing, SQLite substrate, and composable CLI tools (the `scq` concept). That vision remains relevant for:

- Custom tree-sitter SCIP emitters for languages/templates without existing indexers
- Structural AST pattern queries
- A Rust-native alternative to Dora if Dora proves insufficient

This document scopes the immediate, pragmatic layer: extending Doom's existing dispatch with framework-aware handlers and a semantic index backend. If the broader `scq` toolchain gets built later, it replaces Dora as the index backend — everything else stays the same.

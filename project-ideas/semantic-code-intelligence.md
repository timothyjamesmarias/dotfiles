# Semantic Code Intelligence — Design Notes

A Unix-native code intelligence layer. JetBrains-caliber semantic understanding exposed as composable CLI tools and a versioned data substrate, rather than a monolithic IDE.

## Motivation

Unix tooling is strong on text (grep, ripgrep, sed) and weak on semantics. The editor-side answer today is LSP, but LSPs are:

- Editor-bound (no CLI interface, no piping)
- Ephemeral (rebuild index on every start)
- Single-client (one stdio connection per server)
- Single-language (no cross-boundary understanding)

Current setup already combines three tiers:

1. **Tree-sitter** — syntax-accurate incremental parsing
2. **Ctags (gutentags)** — persisted symbol index, shallow (no relationships)
3. **LSP** — semantic relationships, ephemeral, editor-trapped

The gap is between tiers 2 and 3: **persisted, queryable, composable semantic data** that any Unix tool can consume.

## Non-Goals

- Not rebuilding PSI or competing with JetBrains feature-by-feature
- Not writing new language analyzers from scratch
- Not replacing LSP in the editor (at least initially)
- Not a cloud service, not multi-user

## Design Principles

1. **Data as the integration boundary.** Tools integrate with the data, not with each other via APIs. New producers and consumers are independent.
2. **Unix philosophy.** Small composable programs over one monolithic daemon. One program indexes, another queries, another watches the filesystem, another formats output.
3. **Own every layer.** No hidden magic. Every component understandable, swappable, debuggable.
4. **One substrate per domain.** Separate SQLite files for code-intel, diagnostics, metadata, etc. `ATTACH DATABASE` when queries need to span.
5. **Schema is the public interface.** Versioned, documented, migrated with real migration files.

## Architecture

### Three layers

**1. Indexers (producers)**
- Use existing [SCIP](https://github.com/sourcegraph/scip) indexers where available: `scip-typescript`, `scip-python`, `scip-ruby` (via Sorbet), `scip-java`, `scip-go`, `scip-rust`, community `scip-kotlin`, `scip-php`
- Build lightweight tree-sitter-backed SCIP emitters for template engines: Blade, ERB, Slim, Vue SFC blocks. These need only extract component/partial references and link to host-language symbols — maybe a few hundred lines each.
- Producers emit SCIP (protobuf); a normalizer loads SCIP into the SQLite schema.

**2. Storage layer**
- **SQLite as the durable index**, one file per project (`.code-intel.db` at project root, gitignored).
- **WAL mode** for concurrent readers during writes.
- **FTS5** for fuzzy symbol search (better than ctags).
- **JSON1** extension for structured metadata (hover docs, signatures).
- mmap + OS page cache gives Redis-like latency without Redis.

**3. Query layer**
- **Stateless CLI** (`lsq` or similar) is the minimum viable product. Opens SQLite file, runs query, exits. Cold start <10ms.
- **Optional daemon** adds: warm in-memory cache, push-based invalidation via Unix socket, LSP wrapping for live queries that can't be precomputed.
- **Output is pipeable.** `file:line:col` format by default; `--json` for structured consumers.

### Why SQLite (not Redis, not Postgres)

- Embedded, zero ops, no daemon required for basic use
- File-backed, portable, versionable, blow-away-able
- WAL mode handles the bursty-write / read-heavy pattern of code intel natively
- Single-user local design doesn't need Redis' pub/sub or Postgres' concurrency
- Existing ecosystem: any tool in any language can read the index

Redis becomes interesting only if/when multiple clients need push invalidation. Not needed for v1.

### Index freshness

- Filesystem watcher (fswatch/inotify) triggers per-file reindex on save
- Full reindex on branch switch, on request, or via git hook
- Schema versioning allows format evolution without blowing away existing indexes

## Query Surface

What the tool can answer:

**Symbol queries (cheap, SCIP handles natively)**
- `refs <symbol>` — all references
- `defs <symbol>` — definition site(s)
- `impl <interface>` — implementations
- `callers <function>` — call sites
- `hover <symbol>` — type/signature/docs

**Structural queries (tree-sitter, no semantic analysis needed)**
- `structural '<pattern>'` — AST pattern matching
- Framework presets: `--preset rails-routes`, `--preset laravel-components`

**Impact queries**
- `unused` — symbols with zero references (dead code candidates)
- `impact <symbol>` — transitive callers (blast radius of a change)

**Cross-boundary queries (the JetBrains-killer feature)**
- `template-refs <view>` — controllers rendering this view
- `component-refs <name>` — Blade/Vue/Maizzle component usages
- Framework-aware linking between templates and host-language symbols

## Diagnostics — Out of Scope for v1

Diagnostics are fundamentally different from symbol/reference queries: they're *computed* (require a live analyzer), *time-sensitive*, *stream-shaped*. They don't fit the "static index you query" model.

If added later, likely shape:
- Separate SQLite file (`diagnostics.db`) — never co-mingled with the semantic index
- [SARIF](https://sarifweb.azurewebsites.net/) as the normalization format across analyzers
- Sidecar event channel (Unix socket) for push notifications
- Analyzers invoked by watchers or explicitly, output piped through a SARIF normalizer

For v1, diagnostics stay where they already work well: LSP in neovim, watch-mode compilers in terminals, CI for batch checks.

## Extensibility Surface

Because tools integrate with the data, not with each other, the same substrate feeds:

- Editor plugins (neovim, anything that reads SQLite)
- Shell tools: `find-dead-code`, `impact-analysis`, `semantic-grep`
- CI gates: "fail if this commit references a deprecated symbol"
- LLM context providers: precise structured code context instead of blind file dumps
- Refactoring scripts: `lsq refs X | xargs -I{} my-rename-tool {}`
- Documentation generators
- Codebase metrics / dashboards
- Dead code cleanup tooling

Each of these is a weekend project once the substrate exists. None of them coordinate with each other.

## Open Questions

- **Which languages ship first?** Ruby/Rails is probably the highest-value target given the Rails-heavy workflow. PHP/Laravel and Vue/TS follow. Kotlin is useful but smaller surface.
- **Daemon or pure CLI?** Start stateless; add daemon only if cold-start becomes measurable pain.
- **Neovim integration shape?** Thin Lua wrapper calling the CLI, or replace parts of the existing LSP layer? Start with the former.
- **Template ↔ host language linking.** Blade `<x-foo>` → `App\View\Components\Foo` resolution requires knowing framework conventions. Per-framework config files? Convention-over-configuration with overrides?
- **Multi-project / cross-project queries.** Does a monorepo live in one DB or many? `ATTACH DATABASE` makes either workable.

## Prior Art Worth Studying

- [SCIP](https://github.com/sourcegraph/scip) — Sourcegraph Code Intelligence Protocol; the interchange format
- [LSIF](https://lsif.dev/) — predecessor; mostly superseded by SCIP
- [stack-graphs](https://github.com/github/stack-graphs) — GitHub's cross-repo name resolution (Rust, open source)
- [rust-analyzer + salsa](https://github.com/salsa-rs/salsa) — incremental computation framework
- [Sourcegraph's query architecture](https://sourcegraph.com/blog) — how they scale SCIP querying
- [SARIF](https://sarifweb.azurewebsites.net/) — static analysis results format, if diagnostics become in-scope

## Phased Path (if this gets built)

**Phase 0 — Prove the query UX**
- Stateless CLI that opens an existing SCIP file and answers `refs`, `defs`, `impl`
- One target language (pick whichever ships a quality SCIP indexer and is used daily)
- No SQLite yet; just read SCIP protobuf directly

**Phase 1 — Add the substrate**
- Normalizer loads SCIP into SQLite with a real schema
- FTS5 index on symbol names
- CLI queries go through SQLite
- Filesystem watcher triggers per-file reindex

**Phase 2 — Fill the template gap**
- Tree-sitter-backed SCIP emitter for one template engine (likely ERB or Blade first)
- Cross-boundary query: `template-refs`

**Phase 3 — Structural queries**
- Tree-sitter pattern CLI alongside the semantic CLI
- Preset library for common framework patterns

**Phase 4 — Daemon (optional)**
- Warm cache in-memory
- Unix socket for push invalidation
- LSP wrapping for live queries

Each phase is independently useful. None commit to the next.

## Status

Design phase. No code written. Revisit as thinking evolves.

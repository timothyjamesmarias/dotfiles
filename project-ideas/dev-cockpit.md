# Dev Cockpit — Design Notes

A multi-stack command and automation layer for web and backend development. Transient menus over stack-specific CLIs, with a shared vocabulary that works identically whether the project is Spring Boot, Laravel, or Rails.

Same architectural pattern as the [Mobile Dev Cockpit](mobile-dev-cockpit.md) — thin CLI adapters, transient menus, compositions — generalized across web and backend stacks, with agent exposure as a first-class concern.

## Motivation

Every stack has its own way to run, test, migrate, check health, and profile. The verbs are the same; the incantations differ completely:

- Start a dev server: `./gradlew bootRun` vs `php artisan serve` vs `bin/rails s` vs `npm run dev`
- Run migrations: `./gradlew flywayMigrate` vs `php artisan migrate` vs `bin/rails db:migrate`
- List endpoints: curl Actuator `/mappings` vs `artisan route:list --json` vs `rails routes`
- Profile: attach async-profiler to a PID grepped from `jps` vs toggle Xdebug profiler mode vs drop `stackprof` into middleware

A developer working across a Spring Boot API, a Laravel monolith, and a Rails app rebuilds muscle memory for each. There is no shared vocabulary and no shared tool surface.

Profiling and observability are especially painful. JVM profiling is `async-profiler -d 30 -f out.html -p $(jps | grep Application | cut -d' ' -f1)` — exactly the kind of ceremony a porcelain eliminates.

The agent gap is the other motivator. An LLM agent helping debug a Spring Boot app has no programmatic way to know which endpoints exist, which Gradle tasks are available, or how to trigger a migration. Everything is implicit knowledge baked into the developer's head. Agents need a discoverable, structured tool surface — not "read the docs and guess the command."

The mobile cockpit proved this pattern works. The same architecture applies here, with an even richer CLI surface to wrap.

## Non-Goals

- Not replacing IDE-specific debugger UIs (IntelliJ's JVM debugger, Xdebug's step debugger). Those remain irreducible GUI surfaces for step-through debugging.
- Not a build system or deployment tool. Gradle, Composer, Bundler, npm stay authoritative; this sits on top.
- Not a Docker/container orchestration layer. If the stack runs in containers, the adapter shells into the container to run the same commands.
- Not a project scaffolder. `spring init`, `laravel new`, `rails new` are one-time operations outside scope.
- Not attempting feature parity across all stacks simultaneously. Each adapter covers what that stack's CLI surface actually exposes. Spring Boot's adapter will be deeper than Laravel's because its tooling surface is richer. That's fine.

## Design Principles

The mobile cockpit's six principles carry over unchanged:

1. **Menu as API.** Every action is a transient entry first.
2. **Thin wrappers.** Each leaf is a shell command + a buffer for output.
3. **Unified vocabulary across stacks.** Same verbs map to whichever stack CLI is correct for the current context.
4. **Emacs-native output.** Build output through `compilation-mode`, logs in buffers.
5. **Reproducibility over convenience.** Scenario setup lives in versioned files.
6. **Composable, not monolithic.** Shell commands first, Elisp second.

One new principle:

7. **Agent-legible by default.** Every command supports `--json` output. The tool surface is described by a machine-readable manifest. Discoverability is not just for humans at a transient menu; it is equally for LLM agents that need to know what they can do.

## Architecture

### Three layers

**1. Stack adapters (shell)**

Thin shell scripts wrapping each stack's CLI surface into the shared vocabulary:

- `dc-spring <verb> [args]` over `./gradlew`, Actuator, JFR, async-profiler, Flyway
- `dc-laravel <verb> [args]` over `php artisan`, Composer, Laravel Sail
- `dc-rails <verb> [args]` over `bin/rails`, `bundle exec`, Rake
- `dc-node <verb> [args]` (future) over npm scripts, framework-specific CLIs
- `dc` dispatcher that detects the current stack and delegates

Stack detection heuristic:

- `build.gradle.kts` or `build.gradle` + Spring dependencies → Spring Boot
- `artisan` at project root → Laravel
- `Gemfile` + `config/routes.rb` → Rails
- `package.json` + framework markers (next.config, nuxt.config) → Node/web
- Override via `.dir-locals.el` or `.dev-cockpit` dotfile

Detection is heuristic-first, explicit-override-second.

**2. Transient menu layer (Elisp)**

Menu tree rooted at `dev-cockpit`:

- Stack branches (`spring`, `laravel`, `rails`) — each populated by what that adapter actually supports
- Cross-stack verbs (`run`, `test`, `migrate`, `health`, `logs`, `profile`, `endpoints`, `deps`)
- Per-project defaults from `.dir-locals.el`

The menu only shows verbs the detected stack implements. Unlike the mobile cockpit, where iOS and Android have near-symmetric surfaces, web/backend stacks have asymmetric depth.

**3. Automations / compositions (shell + Elisp)**

- **Health dashboard.** Poll health/actuator endpoints across running services, render a summary buffer. Spring: Actuator `/health`, `/info`, `/metrics`. Laravel: health-check route. Rails: `/up` (7.1+).
- **Migration status.** Unified view of pending/applied migrations across Flyway, Laravel migrations, ActiveRecord.
- **Endpoint inventory.** List all HTTP endpoints: Actuator `/mappings`, `artisan route:list --json`, `rails routes`. Normalized to a common format.
- **Profile capture.** Stack-appropriate profiling with a single verb: JFR/async-profiler for Spring, Xdebug/Blackfire for Laravel, `stackprof` for Rails.
- **Dependency audit.** `./gradlew dependencies`, `composer show`, `bundle list` — normalized output, highlight outdated/vulnerable.
- **Log correlation.** If multiple services are running, interleave logs in one buffer with stack-colored prefixes.
- **Semantic code intelligence bridge.** When `lsq` is available, compose queries: chain `dc-spring endpoints --json` with `lsq callers <handler-symbol>` to trace from HTTP route to code to callers.

### Why Emacs (and transient specifically)

Same rationale as the mobile cockpit. `transient.el` is battle-tested for discoverable, composable menus over a large command surface. The web/backend surface is even larger — Spring Boot Actuator alone has a dozen+ endpoints — making discoverability more critical, not less.

### What stays out of Emacs

- Step-through debugging in IntelliJ / Xdebug / `binding.break` — launched and used in their native tools.
- APM dashboards (Grafana, Datadog) — production observability is a different tool. This is dev-local.
- GUI database clients — opened directly when needed.

## Command Surface

Core verbs every adapter implements (where the stack supports it):

| Verb | Meaning | Spring Boot | Laravel | Rails |
|---|---|---|---|---|
| `run` | Start dev server | `./gradlew bootRun` | `php artisan serve` | `bin/rails s` |
| `test` | Run test suite | `./gradlew test` | `php artisan test` | `bin/rails test` / `rspec` |
| `test-file` | Test current file | `--tests` filter | `--filter` | file argument |
| `debug` | Attach debugger | JDWP agent flags | Xdebug enable | `debug` gem |
| `profile` | Performance capture | JFR / async-profiler | Xdebug / Blackfire | stackprof |
| `logs` | Stream app logs | stdout + Actuator `/logfile` | `storage/logs/` tail | `log/development.log` |
| `endpoints` | List HTTP routes | Actuator `/mappings` | `artisan route:list` | `rails routes` |
| `migrate` | Run migrations | Flyway migrate | `artisan migrate` | `rails db:migrate` |
| `migrate-status` | Migration state | Flyway info | `artisan migrate:status` | `rails db:migrate:status` |
| `migrate-rollback` | Revert last | Flyway undo | `artisan migrate:rollback` | `rails db:rollback` |
| `health` | Check app health | Actuator `/health` | health-check route | `/up` (7.1+) |
| `deps` | List/audit deps | `./gradlew dependencies` | `composer show` | `bundle list` |
| `console` | Interactive REPL | `jshell` w/ classpath | `artisan tinker` | `rails console` |
| `clean` | Clean artifacts | `./gradlew clean` | `composer dump-autoload` | `rails tmp:clear` |

Every verb supports `--json` for structured output.

## Spring Boot Adapter (detailed)

Spring Boot is the first target because it has the richest — and most ceremony-heavy — tooling surface.

### Gradle integration

- Build, test, clean via `./gradlew` with compilation-mode regexes for Gradle output
- Task listing (`./gradlew tasks --all`) surfaced in the menu
- Dependency tree (`./gradlew dependencies`) with conflict highlighting
- Test filtering: `./gradlew test --tests "com.example.FooTest.barMethod"`

### Actuator surface

Actuator is the killer feature for a Spring Boot cockpit. All endpoints hit via curl, parsed from JSON:

- `/actuator/health` — health checks with component detail
- `/actuator/info` — app metadata
- `/actuator/metrics/{name}` — JVM and app metrics (heap, GC, HTTP timings)
- `/actuator/mappings` — all request mappings (the `endpoints` verb)
- `/actuator/env` — environment properties for debugging config
- `/actuator/loggers/{name}` — read and change log levels at runtime without restart (POST)
- `/actuator/threaddump` — JVM thread dump
- `/actuator/heapdump` — heap dump download
- `/actuator/flyway` — Flyway migration status

The adapter knows the Actuator base path (configurable, defaults to `/actuator`).

### JFR (Java Flight Recorder)

- `dc-spring profile start` → attach JFR with a sensible default config
- `dc-spring profile stop` → save `.jfr`, open JMC or convert to flamegraph
- Continuous recording mode with event dump on demand

### async-profiler

- `dc-spring profile --mode cpu` → async-profiler attach, produce flamegraph SVG
- `dc-spring profile --mode alloc` → allocation profiling
- Output as flamegraph HTML (browser) or collapsed stacks (for diff between runs)

### Flyway

- `dc-spring migrate` → `./gradlew flywayMigrate` or direct Flyway CLI
- `dc-spring migrate-status` → Flyway info or Actuator `/flyway`
- `dc-spring migrate-rollback` → Flyway undo (if undo migrations exist)
- Migration file creation: generate timestamped SQL file in the right directory

### Spring DevTools

- Detect if DevTools is on the classpath
- Trigger restart via DevTools trigger file (`dc-spring restart`)
- LiveReload integration status

## Laravel Adapter (sketch)

`artisan` is the central CLI surface — most verbs map directly:

- `artisan route:list --json` for endpoints
- `artisan migrate`, `artisan migrate:status`, `artisan migrate:rollback`
- `artisan tinker` for console
- `artisan queue:work` / `artisan queue:listen` for queue management
- `artisan schedule:list` for scheduled tasks
- Composer for deps
- Xdebug / Blackfire for profiling
- Laravel Telescope as an optional richer observability surface (if installed)
- Sail-aware: detect Laravel Sail, prefix commands with `sail` automatically

## Rails Adapter (sketch)

`bin/rails` and `bundle exec` as the command surface:

- `rails routes` for endpoints
- `rails db:migrate`, `rails db:migrate:status`, `rails db:rollback`
- `rails console` for REPL
- Test runner detection: Minitest vs RSpec (presence of `spec/` and `rspec` in Gemfile)
- `stackprof` / `rack-mini-profiler` for profiling
- Puma/Unicorn detection for server management

## Agent Exposure

This is the architecturally novel piece — not present in the mobile cockpit.

### Every command is agent-consumable

All adapters support `--json` output producing structured data with stable schemas:

```json
// dc-spring endpoints --json
[
  {"method": "GET", "path": "/api/users", "handler": "UserController#index"},
  {"method": "POST", "path": "/api/users", "handler": "UserController#create"}
]
```

### Discoverable manifest

- `dc manifest` → full tool surface as JSON: detected stack, available verbs, what each does
- `dc manifest --mcp` → MCP-formatted tool definitions
- Manifest is auto-generated from the adapter surface, not hand-maintained separately
- This is how an agent bootstraps: "what can I do in this project?"

### Semantic code intelligence bridge

When `lsq` (from the semantic-code-intelligence project) is available, the cockpit and code-intel tools compose:

1. `dc-spring endpoints --json` → find the handler for `/api/orders`
2. `lsq callers OrderController.index` → find the call chain
3. `dc-spring profile --mode cpu --json` → profile the endpoint
4. `dc-spring health --json` → check downstream service health

Both projects share the SQLite-as-data-substrate philosophy. An agent can join endpoint data with symbol data when both are available.

### Design constraints for agent consumption

- Commands are stateless (no session to maintain)
- JSON output is parseable without screen-scraping
- Error conditions reported as structured JSON with error codes, not just stderr
- Idempotent where possible (querying health twice gives the same result)

## Phased Path

**Phase 0 — Spring Boot adapter (shell)**
Shell scripts for `dc-spring` covering: `run`, `test`, `logs`, `endpoints`, `health`, `migrate`, `deps`. All with `--json` from day one. Usable from any terminal. Validate the shared vocabulary against a real project.

**Phase 1 — Stack detection + dispatcher**
`dc` dispatcher with heuristic detection. Add a second adapter (Laravel or Rails) to prove the vocabulary generalizes. If it doesn't, fix it here before building menus on top.

**Phase 2 — Transient menus**
Root `dev-cockpit` transient wrapping Phase 0-1 adapters. `.dir-locals.el` integration. Compilation-mode regexes for Gradle and Artisan/Rails output.

**Phase 3 — Agent manifest + MCP**
`dc manifest`, `dc manifest --mcp`. Auto-generation from adapter surface. Test with an actual LLM agent to validate the tool surface is sufficient and discoverable.

**Phase 4 — Profiling + advanced Actuator**
JFR, async-profiler, Actuator metrics/threaddump/heapdump, runtime log-level changes. The high-value Spring-specific features.

**Phase 5 — Compositions**
Health dashboard, migration status across stacks, endpoint inventory, log correlation, semantic-code-intelligence bridge.

Each phase is independently useful. The project is worthwhile even if it stops at Phase 1.

## Open Questions

- **Vocabulary generalization.** The shared verbs are designed around Spring/Laravel/Rails. Do they stretch to Django, Go, Express? Test against at least one non-target stack before committing.
- **Actuator authentication.** Production Actuator endpoints are (should be) secured. Dev-only for v1; production observability is a different tool.
- **Container awareness.** If the app runs in Docker, does `dc-spring` shell into the container or hit exposed ports? Probably configurable. Same question for Laravel Sail.
- **Multi-service projects.** A repo with a Spring Boot API and a React frontend. Does `dc` detect both? Does the menu show both? Probably: detect all stacks, let the user scope to one or fan out.
- **MCP protocol stability.** MCP is evolving. Ship the manifest as plain JSON; MCP formatting is a view on top.
- **Overlap with semantic-code-intelligence.** Both projects answer "what endpoints exist" — the cockpit asks the running app (Actuator), code-intel asks the source code (SCIP). Independent data, composable queries.
- **Gradle vs Maven.** Spring adapter assumes Gradle. Maven is common. Gradle-only for v1; add Maven later if needed.

## Prior Art Worth Studying

- [Magit](https://github.com/magit/magit) / [transient.el](https://github.com/magit/transient) — the menu pattern
- [Spring Boot Actuator](https://docs.spring.io/spring-boot/reference/actuator/) — the API surface the Spring adapter wraps
- [async-profiler](https://github.com/async-profiler/async-profiler) — JVM profiling, flamegraph output
- [JFR](https://docs.oracle.com/en/java/javase/21/jfr/) — built-in JVM profiling
- [Flyway](https://flywaydb.org/) — database migration CLI and Gradle plugin
- [Laravel Artisan](https://laravel.com/docs/artisan) — Laravel's CLI surface
- [MCP](https://modelcontextprotocol.io/) — agent tool definition standard
- [just](https://github.com/casey/just) — command runner; similar "unified verbs" idea but without stack detection or agent exposure

## Status

Design phase. No code written. Revisit as thinking evolves.

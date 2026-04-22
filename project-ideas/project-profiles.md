# Project Profiles — Design Notes

Per-project command dispatch for Emacs. Stable verbs across stacks, perspectival
resolution per project. Builds on Projectile; lives in Doom config; no new
package, no new runtime.

Sibling to [Dev Cockpit](dev-cockpit.md), [Mobile Dev
Cockpit](mobile-dev-cockpit.md), and [Semantic Code
Intelligence](semantic-code-intelligence.md), but scoped narrowly as the
Emacs-side dispatch layer. Where the cockpits are frontend-agnostic substrates
with Emacs porcelains on top, this layer is inherently an Emacs concern and is
implemented as Emacs code, end to end.

## Motivation

Projectile already dispatches a small set of project-level verbs —
`projectile-run-project`, `-test-project`, `-compile-project` — with per-type
defaults registered via `projectile-register-project-type`. This is the right
shape. It is also small: ~5 built-in verbs, per-project customization squeezed
through `.dir-locals.el`, and no first-class concept of "this Rails app uses
Docker; the other one doesn't."

JetBrains-style IDEs solve this with run configurations bound to UI slots. The
trade-off is that every project reshuffles the UI and muscle memory is fragile.
Emacs can do better: keep the keybinding and the verb stable, let the resolved
command shift perspectivally based on which project you are in.

The gap this layer fills:

- **Wider vocabulary.** Beyond `run`/`test`/`compile`, stacks have meaningful
  verbs like `migrate`, `console`, `logs`, `seed`, `routes`, `deep-link`,
  `screenshot`. Projectile does not cover these.
- **Ergonomic per-project overrides.** `.dir-locals.el` works but is awkward
  for expressing "this project uses Docker; call `docker compose exec web
  rails s` instead of `bin/rails s`." Want a first-class profile format.
- **Stable surface across projects.** `SPC c r` should always mean "run this
  project." What it resolves to varies. Muscle memory is preserved; the
  intelligence is in resolution, not presentation.

## Non-Goals

- Not a cross-editor tool. This is Emacs-only. The frontend-agnostic
  substrates (Dev Cockpit, Semantic Code Intelligence) exist for cases where
  other consumers need the data. Project-profile dispatch has no meaningful
  output outside the editor firing the command.
- Not a replacement for Projectile. Projectile keeps owning project
  detection, root resolution, file indexing, and its own commands. This layer
  extends Projectile, not replaces it.
- Not a standalone package. At least not yet. Lives in `config.el` while it
  evolves. If it stabilizes and feels generic, extract later — not before.
- Not a runtime/environment manager. Environment activation (Ruby version,
  Node version, `.env` loading) stays with direnv / mise / asdf. Profiles
  describe *what commands to run*, not how to set up the environment those
  commands assume.
- Not a build system or task runner. Make, Rake, Gradle, npm scripts remain
  authoritative. Profiles dispatch to them; they do not replace them.

## Design Principles

1. **Dispatch, not exposure.** Verbs are stable and always defined. Resolution
   returns a command, or null. Presentation (menus, keybindings) filters null
   cases cosmetically; the data model does not.
2. **Stack + project, two layers.** A stack profile (rails, laravel, go, vue,
   spring, android, ios) declares the default commands. A project profile
   overlays overrides for a specific repo. Project wins where specified;
   stack fills the rest.
3. **Versioned repo config + user-local overrides.** Runtime choices that are
   personal or environmental (Docker vs native, ports, DB URLs) live in a
   user-local file. Team-shared project facts (test command, migrate command)
   live in the repo. Precedence: user-local > repo > stack default.
4. **Muscle memory is stable; commands are perspectival.** Keybindings never
   vary per project. The command dispatched under them does.
5. **Projectile as root source.** `projectile-project-root` and
   `projectile-project-type` are the canonical signals. Do not reinvent
   detection.
6. **Declarative first; callable escape hatch.** Profile values are strings by
   default. Where a lambda is genuinely needed (rare), allow it — with the
   same safety model as `safe-local-variable-values`.

## Architecture

### Data model

A verb registry and a resolver. The registry lists every verb the system
knows about. The resolver, given a project context, returns the resolved
command for each verb, or null.

```elisp
(defvar +tim/project-verbs
  '(run test migrate seed console logs routes
    lint format build deploy open-docs)
  "All known verbs. Stable vocabulary, expanded deliberately.")
```

Stack defaults registered per-type. Example shape:

```elisp
(+tim/register-stack 'rails
  :run     "bin/rails server"
  :test    "bin/rspec"
  :migrate "bin/rails db:migrate"
  :console "bin/rails console"
  :routes  "bin/rails routes")
```

Project profile at repo root (`.project-profile.el` or similar). Read via
`read`, data only (with the callable escape hatch above). Example:

```elisp
;; .project-profile.el
((stack . rails)
 (run     . "docker compose exec web bin/rails server")
 (test    . "docker compose exec web bin/rspec")
 (migrate . "docker compose exec web bin/rails db:migrate"))
```

User-local override at `~/.config/emacs-project-overrides/<project-hash>.el`
for personal/environmental deltas that should not be versioned.

### Resolution

```
(verb, project) → walk precedence:
  1. user-local override file (if exists)
  2. repo .project-profile.el (if exists)
  3. stack defaults (for projectile-project-type)
  → return command string, or nil
```

Typed nulls worth distinguishing (for UX and future agent surface):

1. **Not applicable to stack** — `deep-link` on a Rails API.
2. **Applicable but unconfigured** — stack supports it, project hasn't filled it in.
3. **Explicitly disabled** — profile declares the verb inert for this project.

### Presentation (porcelain)

- Transient menu under a single prefix (e.g. `SPC c`). Verbs grouped sensibly.
- Null-resolved verbs hidden in the menu; surfaced distinctly in a
  `project-profile doctor`-style command ("defined but not configured here").
- Which-key labels come from the verb registry, not per-profile — stays
  stable across projects.
- Output routed through `compilation-mode` for runnable commands, `comint`
  for interactive (console, logs).

## Integration points

- **Projectile.** Root detection, project-type detection. Reuse
  `projectile-project-root` and `projectile-project-type`.
- **Doom workspaces (persp-mode).** Orthogonal. Workspaces scope *buffers*;
  profiles scope *commands*. Both can coexist. The C-n/C-p buffer cycle
  already lives at the intersection of project × workspace (see
  `+tim/project-workspace-buffers` in `config.el`); profiles operate at a
  different layer and do not affect that.
- **Dev Cockpit (future).** If Dev Cockpit materializes, its verbs and this
  layer's verbs will overlap. Plan to converge vocabulary so the eventual
  migration is "swap the resolver, keep the bindings." Until then, keep verb
  names identical where they already exist in the cockpit docs.
- **Semantic Code Intelligence (future).** No direct overlap. Profiles
  dispatch commands; code intelligence answers structural queries. Separate
  layers.

## Incremental build path

No big-bang. Ship small, let friction drive growth.

**Phase 0 — do nothing new yet.** Register current stacks using
`projectile-register-project-type` with `:compile` / `:test` / `:run`.
Lean on projectile's built-in dispatchers. Use `.dir-locals.el` for the
handful of per-project overrides needed today. Validate that this genuinely
hits its ceiling before writing a line of custom code.

**Phase 1 — custom verb layer.** When projectile's 5-verb surface grates
(first time you reach for `migrate` or `logs` and it's not there), add the
`+tim/project-verbs` registry, per-stack registration, and the resolver. One
transient menu. No profile files yet — stack defaults only, per-project
overrides still via `.dir-locals.el`.

**Phase 2 — repo profile file.** Add `.project-profile.el` support when the
dir-locals approach becomes annoying (likely around the second or third
project that needs significant override).

**Phase 3 — user-local overrides.** Add the user-local override tier the
first time you want a setting that must not be committed (Docker vs native
being the obvious trigger).

**Phase 4 — doctor / introspection.** Add a command that reports:
current project, detected stack, profile sources in precedence order, fully
resolved verb table, unconfigured verbs. Useful for debugging profile
resolution and for eventual LLM agent hand-off.

## Open questions

- Naming — `.project-profile.el` is descriptive but long. Alternatives:
  `.dir-profile.el`, `.emacs-project.el`, or reusing `.dir-locals.el` with a
  dedicated top-level key.
- Whether to support profile inheritance beyond stack-default + project-overlay
  (e.g., "this project extends a team-shared base profile"). Probably no until
  there is a concrete case.
- Whether the user-local override file should key by path, by git remote, or
  by project-hash. Path is simplest but fragile across machines; remote is
  portable but wrong for projects with multiple worktrees; hash of
  `projectile-project-root` basename is a reasonable middle.

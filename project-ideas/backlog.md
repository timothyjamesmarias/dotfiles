# Project Ideas Backlog

Sketches of future Emacs porcelains / tools, not yet fleshed out. Each entry
is a one-paragraph prompt for a future dedicated design doc. Graduated ideas
move to their own `project-ideas/<name>.md`.

All of these share a lens: **read a structured stream from something
external, present it richly in Emacs, let me navigate it with keystrokes.**
That is the repeating pattern across Dev Cockpit, Mobile Dev Cockpit,
Semantic Code Intelligence, and Project Profiles — and it will keep being
the right frame.

## High daily-value candidates

### Database porcelain

Cockpit-style layer over `psql`, `mysql`, `sqlite3`, `redis-cli`. Per-project
connection configs (drops into Project Profiles as a `db` verb), query
history per project, schema browser, result sets in a `tabulated-list-mode`
buffer with evil-native navigation. The biggest current-tools gap vs
JetBrains (DataGrip). Works across all backend stacks.

Hardest part: result-set rendering for wide tables without it feeling worse
than `psql` at the terminal.

### Structured log viewer

Parse JSON logs, filter by level/field/request-id, correlate across
services, bookmark events, jump between log and source. Consumes structured
output from Dev Cockpit's `logs` verb. `tail -f` in `compilation-mode` is
the ceiling today; this blows past it.

Likely the highest-daily-value idea for backend debugging. Hardest part:
handling high-volume streams without freezing Emacs — probably wants a
small external helper that pre-filters.

### Test result navigator

Failed tests as a navigable list, jump-to-failure with one keystroke,
snapshot diff viewer, flaky-test heat map over time, per-file pass/fail
indicator in the gutter. `compile-goto-error` is the primitive; everything
above that is absent. Natural hand-off from `dev-cockpit test` / Project
Profiles `test` verb.

Hardest part: unifying test-runner output across stacks (RSpec, JUnit, Go
test, Jest) — probably via a small per-runner adapter that normalizes to a
common schema, same pattern as the cockpit adapters.

## Medium-ceiling candidates

### HTTP / API client cockpit

Postman/Insomnia replacement. `verb.el` or `restclient.el` as the substrate.
The missing layer: collections per project, env-var resolution from Project
Profile (`$BASE_URL` comes from the profile, not an org drawer), response
piping through `jq`, saved response diffing over time, auth token refresh
flows. Text files in, structured results out, everything greppable — strong
composability fit.

Hardest part: auth flows (OAuth refresh, cookie jars) without reinventing a
browser.

### PR review porcelain

Loading a PR diff, threaded comments pinned to hunks, navigating review as
a flow, posting replies back to GitHub — cohesively. Forge has pieces but
no unified review UX. Where Emacs loses hardest to the browser today for
anyone who reviews code seriously.

Hardest part: comments-on-hunks UI and round-tripping review state. Big
project; worth doing only if review volume justifies it.

## Low-ceiling, high-frequency wins

### Per-project shell session manager

Since tmux is out: "open or switch to the shell for this project" as one
keystroke, auto-named vterm buffers, persistent across Emacs restarts,
optionally one-per-workspace. Roughly 50 lines of Elisp. Boringly useful
every day.

Hardest part: nothing. This is the cheapest win on the list.

### Layout profiles

Per-task window layouts within a workspace. "Debug layout" = logs bottom,
test runner right; "writing layout" = single window with `olivetti-mode`.
Doom's workspaces scope buffers but not arrangements — this is often what
people actually mean when they say "perspective." Store layouts as named
window configurations, switch by key.

Hardest part: reliably restoring layouts when buffer set changes (some
buffers closed, some new). Graceful degradation matters.

## Ordering

If betting on daily-life impact, rough priority:

1. Structured log viewer
2. Test result navigator
3. Database porcelain
4. Per-project shell session manager (cheap, ship early)
5. HTTP / API client cockpit
6. Layout profiles
7. PR review porcelain (only if review volume warrants)

None of these should be pre-built. Graduate an idea to its own design doc
only when daily friction makes the absence concrete — same rule as
Project Profiles' phased build path.

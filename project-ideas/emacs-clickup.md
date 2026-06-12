# Emacs ClickUp Client — Design Notes

A fast, search-first Emacs interface for ClickUp, backed by a decoupled API client and a local caching/index layer. The goal is to escape ClickUp's web UI for day-to-day read/search/triage work.

## Motivation

ClickUp's web UI is the bottleneck, not the data:

- **Content layout shift** — blank screen → reflow → content, on nearly every navigation.
- **Slow, bad search** — the single biggest pain point. Finding anything is genuinely hard.
- **Hierarchy-bound navigation** — you must physically walk Workspace → Space → Folder → List → Task to find things, because there's no fast flat view.

This is the classic "data is fine, the UI is the problem" tool. Pulling it into a fast text interface with a zero-latency local index is a categorically different experience.

## Scope

**Primary:** search and find, heavy reads. A local full-text index you can `completing-read`/grep over with zero latency is the killer feature — it directly inverts ClickUp's worst trait.

**Secondary (wanted, not v1):** write capabilities — change task status, write/edit task descriptions, post comments. Deliberately phased after the reader works, because writes bring conflict reconciliation and optimistic-update complexity that the read path doesn't have.

**Explicitly the headline win:** flat "show me everything matching X, now." Hierarchical browsing is nice but secondary.

## Architecture

Three layers, intentionally decoupled.

### 1. API core (decoupled, could live outside Emacs)

Build our own client rather than wrapping an existing CLI (see decision below). ClickUp's API is plain JSON over HTTP with a **personal API token** (single header — auth is nearly free), so the core surface is small:

- Thin request fn (token header + base URL + verb)
- Pagination handling
- Rate-limit backoff (read `X-RateLimit-Remaining` / `X-RateLimit-Reset`, self-pace)
- Response normalization into the cache's shape

The data model is deeply hierarchical: Workspace (team) → Space → Folder → List → Task → Subtask, plus orthogonal custom fields, tags, statuses, members. Note the v2/v3 split (docs/chat are creeping into v3).

**Decoupling fork:** if we want the core *truly* reusable outside Emacs, the clean split is a small local **sidecar/daemon** (Python or Go) that owns the cache + index and exposes a tiny local interface, with Emacs as just one frontend. Heavier, but gives a persistent warm cache that survives Emacs restarts. Otherwise, own it in elisp.

### 2. Caching layer (the heart of the project)

Caching isn't an afterthought here — rate limits (100/min on most plans) make it load-bearing, not just a nicety. Two distinct cache pressures, and treating them as one staleness policy will make it feel either stale or slow:

- **Structure** (spaces, folders, lists, custom-field definitions, members) — changes rarely. Cache aggressively, long TTL.
- **Task content** — changes constantly. Short TTL or explicit invalidation.

Design principles:

- **Optimistic display** is the magic: show cached data instantly, kick off a background refresh, update in place. The inversion of ClickUp's blank-screen-then-shift problem.
- **Blunt "clear everything"** command as a v1 escape hatch, but staleness-aware refresh is the real target.
- **Persistence decision (early):** in-memory (simple, dies with session) vs. on-disk (sqlite or serialized blob). On-disk is what enables *instant* full-text search across everything — likely worth it given that search is the whole point.

### 3. Emacs frontend

- Custom major mode rendering ClickUp data — **not** org-mode sync. Sync-to-org is a known tar pit; a custom mode keeps us in control. (Org sync is seductive — free editing UI — but the reconciliation cost isn't worth it.)
- Flat search view over the local index as the primary entry point.
- Hierarchical browse as secondary.

## Key Decisions

### Build our own client, don't wrap an existing CLI

There's **no official ClickUp CLI**, and nearly every recent community CLI is built "for AI agents," not humans — thin pass-throughs that expose every endpoint but do none of the batching/flattening/indexing we actually care about. Reasons against wrapping:

1. AI-agent CLIs optimize for the wrong thing (clean per-endpoint commands for an LLM), not our cache/search goal.
2. Process-spawn-per-call fights the "instant" premise, especially the bulk "pull the whole workspace into an index" pass.
3. We re-parse their stdout JSON anyway — the API *is* just JSON over HTTP, so the CLI saves no meaningfully hard step. The hard parts (pagination, backoff, custom-field decoding) live in our cache layer regardless.

Wrapping a CLI would only pay off if auth were painful — but the personal token removes that reason entirely.

**Mine these repos as reference, not dependency:**

- [blockful/clickup-cli](https://pkg.go.dev/github.com/blockful/clickup-cli) — Go, ~134/135 endpoints. Best map of the full API surface.
- [nicholasbester/clickup-cli](https://github.com/nicholasbester/clickup-cli) — all ~130 endpoints.
- [triptechtravel/clickup-cli](https://github.com/triptechtravel/clickup-cli) — more human-oriented (Homebrew, status updates, GitHub PR linking).

If we go the sidecar route, [clickup-python-sdk](https://pypi.org/project/clickup-python-sdk/) is the most maintained library (v2.0.1, Apr 2025).

### Read-first, write-second

A read/search/browse tool delivers ~80% of the pain relief with none of the write-path complexity. Writes (status, comments, descriptions) are a second phase with their own concurrency/reconciliation concerns.

### Polling, not webhooks

ClickUp supports webhooks, but they need a public endpoint — awkward for a local tool. Polling + manual refresh is realistic, which loops back to why the cache design matters so much.

## Development Environment

Build against a **throwaway ClickUp Free Forever workspace** — never the employer's production account.

- Free tier includes **full public API access** via personal token (no paid plan / OAuth app needed). Same v2/v3 surface as production.
- Rate limit: **100 req/min per token** on Free/Unlimited/Business (Business Plus = 1,000; Enterprise = 10,000). Dev env is *more* constrained than likely production — the right way around. If backoff feels good at 100/min, it'll feel great anywhere.
- 429s return `X-RateLimit-Remaining` / `X-RateLimit-Reset` headers to drive backoff.

### Dev loop

1. **Seeder script** — script creation of spaces/folders/lists + a few hundred tasks with varied statuses, custom fields, tags, comments. Doubles as the first real exercise of the *write* path. Free tier lets us destructively re-seed without fear.
2. **Record/replay fixtures** — capture real API responses once, develop cache/search/UI against saved JSON. Only hit live API when testing the fetch layer specifically. Sidesteps the 100/min ceiling during normal iteration; makes tests deterministic.
3. **Provoke 429s on purpose** — the Free cap makes it easy to actually test backoff behavior (hard to do against a generous Enterprise limit). Throttle should self-pace off `X-RateLimit-Remaining`, not blindly fire and catch.
4. **Token hygiene from day one** — personal token in env var / `auth-source`, never in code. Go-live = swap the token (real workspace), nothing else changes.

Arc: throwaway free workspace → seeder → record fixtures → build cache/search/UI against fixtures → occasionally validate live → swap token for real workspace.

## Open Questions / Next Steps

- Dig into the API shape that most determines the design: **pagination model, custom-field decoding, and the search/filtered-view endpoints**. These three drive how the cache and "find anything fast" layer get built.
- Decide elisp-native core vs. sidecar daemon (persistence + reusability tradeoff).
- Decide cache persistence: in-memory vs. on-disk (sqlite) — likely on-disk for instant search.

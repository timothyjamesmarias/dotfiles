# Mobile Dev Cockpit — Design Notes

An Emacs-native command and automation layer for iOS, Android, and Kotlin Multiplatform workflows. Transient menus over the platform CLIs, with higher-level utilities composed from those primitives.

## Motivation

Mobile dev is the domain where Unix + Emacs ergonomics most obviously fall off. The tools *are* CLI-reachable — `xcodebuild`, `simctl`, `devicectl`, `xcresulttool`, `xctrace`, `adb`, `./gradlew`, `emulator` — but the ceremony is hostile:

- UDIDs and bundle IDs grepped out of JSON blobs on every invocation
- Per-platform flag soup, with no shared vocabulary between iOS and Android
- Build output that doesn't integrate with `compilation-mode` / `next-error`
- Test results locked in `xcresult` bundles and Gradle XML
- Repro state (deep links, locale, push payloads, launch args) set up by clicking through simulator menus every time
- Nothing fans out across platforms — "install and run on every booted target" is a 6-command recipe

The existing answer is to live in Xcode/Android Studio for the mobile loop and accept the context switch. That works, but it fragments the dev environment for projects (especially KMP) where most of the code is platform-neutral and would otherwise be a normal Emacs project.

## Non-Goals

- Not replacing Xcode's view debugger, memory graph debugger, or Instruments UI. Those remain the irreducible GUI surfaces.
- Not replacing the `.xcassets` / `.xcdatamodeld` / storyboard bundle editors.
- Not a build system. `xcodebuild`, `gradle`, and Amper stay authoritative; this sits on top.
- Not cross-platform in the "one binary for Linux/Mac" sense. macOS-first, because that's where the iOS tooling lives.
- Not a new DSL or config language. Project defaults live in `.dir-locals.el` / plain shell scripts.

## Design Principles

1. **Menu as API.** Every action is a transient entry first. Automations are compositions of menu actions, not separate one-off scripts. This makes the tool discoverable and makes utilities trivial to add.
2. **Thin wrappers.** Each leaf is a shell command + a buffer for output. No re-implementations of what the platform CLIs already do.
3. **Unified vocabulary across platforms.** Same verbs (`install`, `run`, `stop`, `logs`, `screenshot`, `deep-link`, `clear-data`) map to whichever platform CLI is correct for the current context. Memorize one menu, not two toolchains.
4. **Emacs-native output.** Build output flows through `compilation-mode` with regexes for `xcodebuild` and `gradle`. Test failures flow through it too, via `xcresulttool --format json` and Gradle XML parsing. Logs are buffers you live in, not terminal windows.
5. **Reproducibility over convenience.** Scenario setup (deep links, locale, push payloads, launch args) lives as versioned files in the repo. Clicking through the simulator is never the canonical path.
6. **Composable, not monolithic.** Shell commands first, Elisp second. Any util should be runnable outside Emacs so CI and ad-hoc use stay viable.

## Architecture

### Three layers

**1. Platform adapters (shell)**
Thin shell scripts or functions wrapping the platform CLIs into a common vocabulary:
- `mdc-ios install <bundle> <target>`, `mdc-ios launch <bundle> <target>`, `mdc-ios logs <bundle>`, `mdc-ios deep-link <url>`, `mdc-ios screenshot`, `mdc-ios clear-data <bundle>`, etc.
- Matching `mdc-android` surface over `adb` + `emulator` + `gradle`.
- A `mdc` dispatcher that picks the right adapter based on current project / selected target.

These are plain shell. Runnable from any terminal, from CI, from Emacs. No Elisp dependency.

**2. Transient menu layer (Elisp)**
A `magit`-shaped menu tree:
- Root (`mobile-dev`) with platform branches (`ios`, `android`), cross-platform actions (`parity`, `scenario`, `profile`, `devices`), and project-level actions (`build`, `test`, `clean`).
- Each leaf calls an adapter command via `compile` or `make-process`, routing output to `compilation-mode` or a dedicated buffer.
- Per-project defaults (scheme, simulator, flavor, device) resolved from `.dir-locals.el`.

**3. Automations (shell + Elisp)**
Utilities composed from layers 1 and 2. Each is independently small:
- **Parity / fan-out.** One keybind installs + launches on every booted target (iOS sim, Android emulator, physical device) for visual side-by-side regressions.
- **Scenario sweeps.** A list of deep links / locales / themes in a repo file; utility walks through them, screenshots each, dumps to a dated folder.
- **State bookmarking.** Snapshot the simulator's data container (`simctl get_app_container`) + a tag + the current log view. Restore later. Tar+untar under a nice transient face.
- **Compilation integration.** Error regexes for `xcodebuild` and `gradle` output so `next-error` jumps work. `xcresult` → compilation buffer so test failures are clickable.
- **Flaky test triage.** Rerun last-failed tests N times via `xcodebuild test-without-building -only-testing:` or the Gradle equivalent; report pass rate; isolate flakes.
- **Build diff.** Archive two branches, diff app size / method count / Info.plist / declared permissions / embedded frameworks.
- **Log bookmarks.** Tag points in the log stream; filtered views saved as replayable buffers. Halfway between shell history and scratch files.
- **Decoupled profiling.** `profile <scenario.sh>` starts `xctrace` recording, runs the scenario, saves the `.trace`, opens Instruments only at the end as a viewer.

### Why Emacs (and transient specifically)

- `transient.el` is battle-tested for exactly this shape: discoverable, composable menus over a large command surface. Magit proves the pattern scales.
- `compilation-mode`, `comint`, and `vterm` already solve the output ergonomics.
- `.dir-locals.el` solves per-project configuration without a new config format.
- Every utility added gets a keybind and menu entry for free — discoverability scales with the tool surface, opposite of accumulating shell aliases nobody remembers.

### What stays out of Emacs

- The view debugger and memory graph debugger — still launched via Xcode when needed. This is rare enough that the context switch is tolerable.
- Instruments as a viewer — launched only to read `.trace` files produced by CLI profiling runs.
- Bundle-format editors (`.xcassets` / `.xcdatamodeld` / storyboards) — opened directly via `open`, which routes to Xcode. No workflow integration needed.

## Command Surface (first pass)

Cross-platform verbs the menu exposes uniformly:

- `devices` — list booted / connected targets across platforms
- `build` — build for current target, current flavor/scheme
- `install` / `uninstall` — push/remove the built app
- `run` / `stop` — launch / kill
- `logs` — streaming log filtered to bundle ID, in a buffer
- `test` — run tests, results routed through compilation-mode
- `screenshot` / `record` — capture device/sim output
- `deep-link <url>` — route a URL through the running app
- `clear-data` — wipe app state without uninstalling
- `push <payload>` — simulate a push notification (iOS: simctl; Android: adb shell am broadcast)
- `shell` — drop into the appropriate CLI context (`lldb` attached / `adb shell`)

Cross-platform compositions:

- `parity <verb>` — apply verb to every booted target
- `scenario <file>` — replay a recorded scenario
- `bookmark` / `restore <tag>` — state snapshots
- `profile <scenario>` — xctrace / systrace wrapper
- `diff <branch-a> <branch-b>` — build-artifact diff

## Phased Path (if this gets built)

**Phase 0 — Platform adapters**
Shell scripts for `mdc-ios` and `mdc-android` covering the core verbs (build, install, run, logs, deep-link, screenshot, clear-data). Usable from any terminal. No Emacs yet. This is the foundation; if the adapter vocabulary is wrong, fix it here before building a menu on top.

**Phase 1 — Transient menus**
Root `mobile-dev` transient with platform subtrees wrapping Phase 0 commands. `.dir-locals.el` integration for per-project defaults. Output routed through `compilation-mode` with regexes for `xcodebuild` and `gradle`.

**Phase 2 — xcresult / Gradle test integration**
`xcresulttool --format json` → compilation buffer. Gradle test XML parser → compilation buffer. `next-error` jumps to failing test sources.

**Phase 3 — Cross-platform compositions**
`parity`, `scenario`, `bookmark`. These are the payoff — they exist only because the adapters speak a common vocabulary.

**Phase 4 — Higher-level utils**
Flaky test triage, build diff, log bookmarks, decoupled profiling. Each is independently useful; none commits to the next.

Each phase is independently useful. The project is worthwhile even if it stops at Phase 1.

## Open Questions

- **Shell vs Elisp boundary.** How much of the adapter logic is bash/zsh vs pushed into Elisp? Default: shell, so utilities work outside Emacs. But some things (per-project defaults, interactive selection) are cleaner in Elisp. Draw the line per-command, not globally.
- **Scenario file format.** Plain shell script (flexible, no new format) vs a declarative file (easier to read, compose, and diff). Leaning plain shell for v0; revisit if scenarios start repeating structure.
- **Target selection UX.** When multiple simulators/devices are booted, how does "the current target" get picked? Default to "most recently used" with an easy override, probably via `completing-read` over `mdc devices` output.
- **KMP-specific shape.** Does a KMP project get a unified `test` that runs shared-module tests across both platforms and correlates results? Or keep platforms separate and let the user fan out via `parity`?
- **Physical device ergonomics.** `devicectl` is newer and less documented than `simctl`. How much of the iOS menu works on real devices in v1 vs sim-only?
- **Project detection.** Heuristic (presence of `*.xcodeproj`, `build.gradle.kts`, `shared/` module) vs explicit declaration in `.dir-locals.el`. Probably both — heuristic with override.

## Prior Art Worth Studying

- [Magit](https://github.com/magit/magit) — the canonical example of transient-over-CLI done well
- [transient.el](https://github.com/magit/transient) — the menu library itself
- [xctool](https://github.com/facebookarchive/xctool) — earlier attempt at smoothing `xcodebuild`
- [fastlane](https://fastlane.tools/) — covers distribution/signing automation; overlapping surface but different shape (Ruby DSL, monolithic)
- [xcbeautify](https://github.com/cpisciotta/xcbeautify) — output formatter, usable as-is inside compilation-mode
- [Maestro](https://maestro.mobile.dev/) — UI test scenario replay; conceptually close to scenario sweeps
- [idb](https://github.com/facebook/idb) — Facebook's cross-device iOS CLI; worth mining for adapter vocabulary

## Status

Design phase. No code written. Revisit as thinking evolves.

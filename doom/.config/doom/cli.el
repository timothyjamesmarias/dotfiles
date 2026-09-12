;;; cli.el -*- lexical-binding: t; -*-
;; Loaded by Doom's CLI (doom sync, doctor, etc.), which runs in batch mode.
;; astro-ts-mode's autoloads call `treesit-ready-p' at top level; treesit isn't
;; preloaded in batch, and the resulting void-function error silently aborts
;; profile init generation (leaving init.<version>.el unwritten, which breaks
;; Emacs startup with "Doom hasn't been initialized yet").
(require 'treesit nil t)

;; `doom env' snapshots whatever environment it runs in, and it has been run
;; from shells inside Emacs/Claude Code before -- capturing a dead EDITOR
;; (deleted Emacs 30 app), GIT_EDITOR=true (Claude Code's marker), stale
;; TERMINFO, and CLAUDE_* session vars that make every spawned `claude'
;; look like a nested child session. Keep all of that out of the snapshot.
(with-eval-after-load 'doom-cli-env
  (setq doom-env-deny
        (append doom-env-deny
                '("^CLAUDECODE$" "^CLAUDE_" "^AI_AGENT$"
                  "^GIT_EDITOR$" "^EDITOR$" "^VISUAL$"
                  "^TERMINFO\\(_DIRS\\)?$" "^EMACS_VTERM_PATH$"
                  "^ALACRITTY_" "^WINDOWID$" "^SECURITYSESSIONID$"
                  "^TERM_PROGRAM\\(_VERSION\\)?$" "^PROOT$"))))

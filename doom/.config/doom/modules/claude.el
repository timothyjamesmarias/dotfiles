;;; modules/claude.el --- Claude Code integration -*- lexical-binding: t; -*-

;; claude-code.el (stevemolitor) drives the `claude' CLI from Emacs. We use
;; the vterm backend so it inherits our vterm tweaks in config.el (jk-escape,
;; drag-n-drop path insertion). The package manages its own per-project
;; buffers named `*claude:<project>*', so there's no hand-rolled launcher
;; anymore -- see git history for the previous vterm wrapper.
;;
;; The `C-c c' command map is enabled globally; the leader bindings below
;; mirror it under `SPC o c' to match the old muscle memory.

(defun +tim/claude-display-buffer-right (buffer)
  "Display the claude code BUFFER in a right-side horizontal split."
  (display-buffer buffer '((display-buffer-in-direction)
                           (direction . right)
                           (window-width . 0.5))))

(use-package! claude-code
  :init
  (setq claude-code-terminal-backend 'vterm
        claude-code-display-window-fn #'+tim/claude-display-buffer-right)
  :config
  (claude-code-mode)
  (map! :leader
        (:prefix ("o c" . "claude")
         :desc "Start / switch"       "c" #'claude-code
         :desc "New in project"       "p" #'claude-code
         :desc "Continue last"        "r" #'claude-code-continue
         :desc "Resume (pick)"        "R" #'claude-code-resume
         :desc "Toggle window"        "t" #'claude-code-toggle
         :desc "Command menu"         "m" #'claude-code-transient
         :desc "Send region/buffer"   "s" #'claude-code-send-region
         :desc "Send with context"    "x" #'claude-code-send-command-with-context
         :desc "Fix error at point"   "e" #'claude-code-fix-error-at-point
         :desc "Slash commands"       "/" #'claude-code-slash-commands
         :desc "Switch to buffer"     "b" #'claude-code-switch-to-buffer
         :desc "Select instance"      "l" #'claude-code-select-buffer)
        (:prefix "o"
         :desc "Continue Claude"      "C" #'claude-code-continue)))

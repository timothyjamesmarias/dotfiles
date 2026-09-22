;;; modules/claude.el --- Claude Code integration -*- lexical-binding: t; -*-

;; claude-code.el (stevemolitor) drives the `claude' CLI from Emacs. We use
;; the ghostel backend (see config.el); the vterm tweaks there (jk-escape,
;; drag-n-drop path insertion) no longer apply. The package manages its own per-project
;; buffers named `*claude:<project>*', so there's no hand-rolled launcher
;; anymore -- see git history for the previous vterm wrapper.
;;
;; The `C-c c' command map is enabled globally; the leader bindings below
;; mirror it under `SPC o c' to match the old muscle memory.

(defconst +tim/claude-inherited-session-vars
  '("CLAUDE_CODE_CHILD_SESSION"
    "CLAUDE_CODE_SESSION_ID"
    "CLAUDECODE"
    "CLAUDE_CODE_ENTRYPOINT"
    "CLAUDE_CODE_EXECPATH")
  "Env vars that mark a `claude' process as a nested child session.
If Emacs was launched from inside a Claude session it inherits these,
and every `claude' we spawn then looks like a child of that long-dead
session -- it writes no transcript and never shows up in /resume.")

(defun +tim/claude-scrub-inherited-session-env (&rest _)
  "Unset inherited Claude session vars for the spawned `claude' process.
Entries without a `=' remove the variable, and `process-environment' is
searched front-to-back, so these shadow whatever Emacs inherited."
  +tim/claude-inherited-session-vars)

;; Claude Code's "edit prompt in $EDITOR" runs emacsclient from the claude
;; buffer; server.el visits the scratch file over the claude window, and on
;; `server-edit' (C-x #) it kills the buffer without restoring where the
;; edit came from -- you land on some unrelated buffer. Remember the claude
;; buffer at visit time and jump back once server.el is done. The jump is
;; deferred with a timer because `server-done-hook' runs before server.el's
;; own final buffer switch, which would clobber a direct switch here.
(defvar-local +tim/claude-server-origin nil
  "Claude buffer this server edit was launched from, if any.")

(defun +tim/claude-server-remember-origin ()
  "Mark the claude buffer as origin when an edit starts in its window."
  (let ((buf (window-buffer (selected-window))))
    (when (string-prefix-p "*claude" (buffer-name buf))
      (setq +tim/claude-server-origin buf))))

(defun +tim/claude-server-return-to-origin ()
  "Return to the claude buffer this server edit came from."
  (let ((origin +tim/claude-server-origin))
    (when (buffer-live-p origin)
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p origin)
                       (if-let* ((win (get-buffer-window origin)))
                           (select-window win)
                         (switch-to-buffer origin))))))))

(add-hook 'server-visit-hook #'+tim/claude-server-remember-origin)
(add-hook 'server-done-hook #'+tim/claude-server-return-to-origin)

;; C-x passthrough: ghostel lists "C-x" in `ghostel-keymap-exceptions', so it
;; never reaches the Claude TUI. Rebinding it buffer-locally in insert state
;; only (via `claude-code-start-hook') keeps C-x as the Emacs prefix in normal
;; state and in every other ghostel buffer. `ghostel--send-encoded' (the same
;; call evil-ghostel's Ctrl-passthrough uses) respects the kitty keyboard
;; protocol Claude Code negotiates, unlike sending the raw 0x18 byte.
(defun +tim/claude-send-C-x ()
  "Send C-x to the Claude terminal."
  (interactive)
  (ghostel--send-encoded "x" "ctrl"))

(defun +tim/claude-enable-C-x-passthrough ()
  "Route insert-state C-x to the Claude TUI in this buffer."
  (evil-local-set-key 'insert (kbd "C-x") #'+tim/claude-send-C-x))

(defun +tim/claude-display-buffer-right (buffer)
  "Display the claude code BUFFER in a right-side horizontal split."
  (display-buffer buffer '((display-buffer-in-direction)
                           (direction . right)
                           (window-width . 0.5))))

(use-package! claude-code
  :init
  (setq claude-code-terminal-backend 'ghostel
        claude-code-display-window-fn #'+tim/claude-display-buffer-right)
  :config
  (add-hook 'claude-code-process-environment-functions
            #'+tim/claude-scrub-inherited-session-env)
  (add-hook 'claude-code-start-hook #'+tim/claude-enable-C-x-passthrough)
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

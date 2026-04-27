;;; modules/claude.el --- Claude Code integration -*- lexical-binding: t; -*-

(defvar +tim/claude-buffer-name "*claude*"
  "Buffer name for the dedicated Claude Code vterm.")

(defun +tim/--claude-require ()
  "Ensure the `claude' CLI is available on PATH."
  (unless (executable-find "claude")
    (user-error "`claude' CLI not found on PATH")))

(defun +tim/--claude-launch (cmd &optional dir)
  "Open a dedicated vterm buffer running CMD in a right-side split.
If DIR is non-nil, use it as `default-directory'.
If the buffer already exists with a live process, just switch to it."
  (+tim/--claude-require)
  (let ((buf (get-buffer +tim/claude-buffer-name)))
    (if (and buf (get-buffer-process buf))
        (if-let ((win (get-buffer-window buf t)))
            (select-window win)
          (select-window (split-window-right))
          (switch-to-buffer buf))
      (when buf (kill-buffer buf))
      (let ((default-directory (or dir default-directory)))
        (require 'vterm)
        (select-window (split-window-right))
        (let ((vterm-buffer-name +tim/claude-buffer-name)
              (vterm-shell cmd))
          (vterm--internal #'switch-to-buffer))))))

(defun +tim/claude ()
  "Open Claude Code in a dedicated vterm popup."
  (interactive)
  (+tim/--claude-launch "claude"))

(defun +tim/claude-resume ()
  "Resume the last Claude Code session in a dedicated vterm popup."
  (interactive)
  (+tim/--claude-launch "claude --resume"))

(defun +tim/claude-project ()
  "Open Claude Code in the project root."
  (interactive)
  (let ((root (or (and (fboundp 'doom-project-root) (doom-project-root))
                  (locate-dominating-file default-directory ".git")
                  default-directory)))
    (+tim/--claude-launch "claude" root)))

(map! :leader
      (:prefix ("o c" . "claude")
       :desc "Open Claude"          "c" #'+tim/claude
       :desc "Resume session"       "r" #'+tim/claude-resume
       :desc "Claude in project"    "p" #'+tim/claude-project)
      (:prefix "o"
       :desc "Resume Claude"        "C" #'+tim/claude-resume))

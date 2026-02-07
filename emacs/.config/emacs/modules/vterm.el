;;; vterm.el --- Terminal integration with vterm -*- lexical-binding: t; -*-
;;
;; Description: vterm configuration for terminal workflows
;;
;;; Commentary:
;; Replaces tmux panes/windows with vterm in Emacs:
;;   tmux panes → vterm buffers
;;   Multiple terminals → multiple vterm instances per project
;;
;; Critical for Claude Code usage across multiple projects!
;;
;;; Code:

;; ========================================
;; vterm - Full Terminal Emulator
;; ========================================

(use-package vterm
  :config
  ;; Shell to use (zsh with your config)
  (setq vterm-shell (executable-find "zsh"))

  ;; Max scrollback
  (setq vterm-max-scrollback 10000)

  ;; Kill buffer when process exits
  (setq vterm-kill-buffer-on-exit t)

  ;; Clear scrollback on clear
  (setq vterm-clear-scrollback-when-clearing t)

  ;; Set environment variables
  (setq vterm-environment '("TERM=xterm-256color"))

  ;; Disable line numbers in vterm (no need for them in terminal)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (setq-local global-hl-line-mode nil)))

  ;; Copy mode keybindings (useful for copying terminal output)
  (define-key vterm-mode-map (kbd "C-c C-c") 'vterm-copy-mode)
  (define-key vterm-copy-mode-map (kbd "C-c C-c") 'vterm-copy-mode-done))

;; ========================================
;; vterm-toggle - Quick Terminal Toggle
;; ========================================

(use-package vterm-toggle
  :after vterm
  :config
  ;; Show vterm at bottom with specific height
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project) ; Per-project vterm instances!

  ;; Height of the vterm window (percentage)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.4))))

;; ========================================
;; Multi-vterm - Multiple Terminal Management
;; ========================================

(use-package multi-vterm
  :after vterm
  :config
  ;; Default vterm buffer name
  (setq multi-vterm-buffer-name "vterm")

  ;; Dedicated terminal buffer per project
  (setq multi-vterm-dedicated-window-height-percent 40))

;; ========================================
;; Project-aware vterm Functions
;; ========================================

(defvar tim/vterm-counter-alist '()
  "Alist of (project-root . counter) for naming vterm buffers.")

(defun tim/get-next-vterm-number (project-root)
  "Get next vterm number for PROJECT-ROOT."
  (let ((counter (or (alist-get project-root tim/vterm-counter-alist 0 nil 'equal) 0)))
    (setf (alist-get project-root tim/vterm-counter-alist nil nil 'equal) (1+ counter))
    (1+ counter)))

(defun tim/vterm-new ()
  "Create a new vterm buffer in current project."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (vterm-num (tim/get-next-vterm-number project-root))
         (buffer-name (format "*vterm:%s-%d*" project-name vterm-num)))
    (vterm buffer-name)))

(defun tim/vterm-project ()
  "Open primary vterm for current project, or create if doesn't exist."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (buffer-name (format "*vterm:%s-1*" project-name))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (switch-to-buffer buffer)
      (progn
        (vterm buffer-name)
        ;; Initialize counter for this project
        (setf (alist-get project-root tim/vterm-counter-alist nil nil 'equal) 1)))))

(defun tim/vterm-toggle-project ()
  "Toggle vterm at bottom for current project."
  (interactive)
  (if (projectile-project-p)
      (vterm-toggle)
    (vterm-toggle)))

(defun tim/vterm-claude-code ()
  "Open or switch to Claude Code vterm for current project."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (buffer-name (format "*vterm:claude:%s*" project-name))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (switch-to-buffer buffer)
      (progn
        (vterm buffer-name)
        ;; Optionally start claude automatically
        ;; (vterm-send-string "claude\n")
        ))))

(defun tim/vterm-switch ()
  "Switch between vterm buffers using completion."
  (interactive)
  (let* ((vterm-buffers
          (seq-filter (lambda (buf)
                        (with-current-buffer buf
                          (eq major-mode 'vterm-mode)))
                      (buffer-list)))
         (buffer-names (mapcar #'buffer-name vterm-buffers)))
    (if buffer-names
        (switch-to-buffer
         (completing-read "Switch to vterm: " buffer-names))
      (message "No vterm buffers open"))))

(defun tim/vterm-kill-all-project ()
  "Kill all vterm buffers for current project."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (pattern (format "^\\*vterm:%s" project-name)))
    (dolist (buffer (buffer-list))
      (when (string-match-p pattern (buffer-name buffer))
        (kill-buffer buffer)))
    ;; Reset counter
    (setf (alist-get project-root tim/vterm-counter-alist nil nil 'equal) 0)
    (message "Killed all vterm buffers for project: %s" project-name)))

;; ========================================
;; Keybindings
;; ========================================

(with-eval-after-load 'evil
  (with-eval-after-load 'general
    (when (fboundp 'tim/leader-keys)
      (tim/leader-keys
    ;; Open/terminal prefix (o for "open")
    "o" '(:ignore t :which-key "open")
    "ot" '(tim/vterm-project :which-key "terminal (project)")
    "oT" '(tim/vterm-new :which-key "terminal (new)")
    "oc" '(tim/vterm-claude-code :which-key "claude code")
    "ov" '(vterm :which-key "vterm")
    "oV" '(tim/vterm-switch :which-key "switch vterm")

    ;; Terminal management
    "ok" '(tim/vterm-kill-all-project :which-key "kill project vterms")))))

;; ========================================
;; Evil Integration for vterm
;; ========================================

;; Some useful keybindings for vterm with evil
(with-eval-after-load 'vterm
  (evil-define-key 'normal vterm-mode-map
    (kbd "p") 'vterm-yank
    (kbd "i") 'evil-insert-state
    (kbd "a") 'evil-insert-state))

;;; vterm.el ends here

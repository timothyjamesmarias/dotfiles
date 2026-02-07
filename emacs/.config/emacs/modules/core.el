;;; core.el --- Core Emacs settings -*- lexical-binding: t; -*-
;;
;; Description: Basic Emacs configuration and sensible defaults
;;
;;; Commentary:
;; This module configures fundamental Emacs behavior:
;; - UTF-8 encoding everywhere
;; - Backup and auto-save file management
;; - Better defaults for editing behavior
;; - macOS-specific integrations
;; - Path/environment inheritance from shell
;;
;;; Code:

;; ========================================
;; Character Encoding
;; ========================================

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; ========================================
;; Backup and Auto-Save Files
;; ========================================

;; Store backup files in a dedicated directory instead of littering
;; the filesystem (similar to how you keep git, tmux configs organized)
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory))))

(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

;; Create backup directories if they don't exist
(make-directory (expand-file-name "backups/" user-emacs-directory) t)
(make-directory (expand-file-name "auto-saves/" user-emacs-directory) t)

;; Version control for backups (keep numbered versions)
(setq version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Don't create lockfiles (already disabled in early-init.el, but enforce here too)
(setq create-lockfiles nil)

;; ========================================
;; Better Defaults
;; ========================================

;; Answer 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Confirm before killing Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Highlight current line
(global-hl-line-mode 1)

;; Column number in mode line
(column-number-mode 1)

;; Line numbers (enable for programming modes only)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Show file size in mode line
(size-indication-mode 1)

;; Visual line mode for text files (word wrap)
(add-hook 'text-mode-hook 'visual-line-mode)

;; ========================================
;; Editing Behavior
;; ========================================

;; Use spaces instead of tabs (respecting .editorconfig when present)
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Wrap long lines
(setq-default truncate-lines nil)

;; Scroll settings (smooth scrolling)
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; Mouse scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

;; Delete selection mode (typing replaces selection)
(delete-selection-mode 1)

;; Auto-revert buffers when files change on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; ========================================
;; Clipboard Integration
;; ========================================

;; Use system clipboard
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

;; ========================================
;; Recent Files
;; ========================================

(use-package recentf
  :straight nil  ; Built-in package
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never))

;; Save recent files periodically
(run-at-time nil (* 5 60) 'recentf-save-list)

;; ========================================
;; Save Place - Remember cursor position
;; ========================================

(use-package saveplace
  :straight nil  ; Built-in
  :init
  (save-place-mode 1)
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory)))

;; ========================================
;; Environment Variables from Shell
;; ========================================

;; Inherit PATH and other env vars from your zsh shell
;; Critical for finding tools like rg, fd, git, etc.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "JAVA_HOME"))
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; ========================================
;; macOS-Specific Settings
;; ========================================

(when (eq system-type 'darwin)
  ;; Use Command as Meta (Option is still available for special chars)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none)

  ;; Smooth scrolling on macOS
  (setq mac-mouse-wheel-smooth-scroll t)

  ;; Open files from Finder in existing frame
  (setq ns-pop-up-frames nil)

  ;; Transparent title bar
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; ========================================
;; Dired Enhancements
;; ========================================

(use-package dired
  :straight nil  ; Built-in
  :config
  ;; Human-readable sizes
  (setq dired-listing-switches "-alh")

  ;; Copy and delete recursively without asking
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'top)

  ;; Reuse same buffer when navigating (like ranger/yazi)
  (setq dired-kill-when-opening-new-dired-buffer t))

;; ========================================
;; Suppress Warnings
;; ========================================

;; Suppress warnings about certain functions (common in modern configs)
(setq warning-suppress-types '((comp)))

;; ========================================
;; Miscellaneous
;; ========================================

;; Increase read output from processes (important for LSP, language servers)
;; Even though you're not using LSP, this is good for other tools
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Don't beep
(setq ring-bell-function 'ignore)

;; Follow symlinks without asking
(setq vc-follow-symlinks t)

;; Prefer horizontal splits (side-by-side) like tmux panes
(setq split-width-threshold 160
      split-height-threshold nil)

;;; core.el ends here

;;; keybindings.el --- Global keybinding migration from tmux -*- lexical-binding: t; -*-
;;
;; Description: Tmux M-* keybindings migrated to Emacs
;;
;;; Commentary:
;; This module preserves your tmux muscle memory by mapping all your
;; M-* (Alt-*) global keybindings to equivalent Emacs commands.
;;
;; Tmux bindings → Emacs equivalents:
;;   M-o  → Find file in project (projectile-find-file)
;;   M-d  → Find directory/file (find-file)
;;   M-r  → Ripgrep in project (projectile-ripgrep / consult-ripgrep)
;;   M-y  → Dired/Dirvish (file manager)
;;   M-n  → New file
;;   M-c  → Toggle vterm (Claude Code)
;;   M-g  → Magit status
;;   M-S  → Project switcher (sessionizer)
;;   M-1 through M-9 → Tab-bar selection
;;   M-Tab → Next tab
;;
;;; Code:

;; ========================================
;; Helper Functions
;; ========================================

(defun tim/find-file-or-project-file ()
  "Find file in project if in a project, otherwise find file normally."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (consult-find)))

(defun tim/ripgrep-or-project-ripgrep ()
  "Ripgrep in project if in a project, otherwise ripgrep from current directory."
  (interactive)
  (if (projectile-project-p)
      (consult-ripgrep (projectile-project-root))
    (consult-ripgrep default-directory)))

(defun tim/new-file ()
  "Create a new file interactively.
Prompts for directory and filename."
  (interactive)
  (let* ((dir (read-directory-name "Directory: " default-directory))
         (filename (read-string "Filename: ")))
    (find-file (expand-file-name filename dir))))

(defun tim/switch-to-tab-or-create (n)
  "Switch to tab N or create it if it doesn't exist."
  (let ((tabs (length (tab-bar-tabs))))
    (if (<= n tabs)
        (tab-bar-select-tab n)
      (tab-bar-new-tab))))

;; Generate functions for M-1 through M-9
(dotimes (i 9)
  (let ((n (1+ i)))
    (defalias (intern (format "tim/tab-%d" n))
      `(lambda ()
         (interactive)
         (tim/switch-to-tab-or-create ,n))
      (format "Switch to tab %d" n))))

;; ========================================
;; Global M-* Keybindings (Tmux Migration)
;; ========================================

(general-define-key
 :states '(normal visual insert emacs)
 :keymaps 'override

 ;; File operations
 "M-o" 'tim/find-file-or-project-file      ; Find file (ff)
 "M-d" 'find-file                           ; Change directory/find file (cdd)

 ;; Search operations
 "M-r" 'tim/ripgrep-or-project-ripgrep     ; Ripgrep (rgf)

 ;; File manager
 "M-y" 'dired-jump                          ; File manager (yazi)

 ;; New file
 "M-n" 'tim/new-file                        ; New file (nf)

 ;; Terminal
 "M-c" 'tim/vterm-toggle-project           ; Claude Code / vterm
 "M-C" 'tim/vterm-claude-code              ; Dedicated Claude vterm

 ;; Git
 "M-g" 'magit-status                        ; Git status
 "M-G" 'magit-dispatch                      ; Git dispatch menu

 ;; Project switching (sessionizer!)
 "M-S" 'tim/project-switch-project         ; Project sessionizer (M-S)

 ;; Tab/Window selection (M-1 through M-9)
 "M-1" 'tim/tab-1
 "M-2" 'tim/tab-2
 "M-3" 'tim/tab-3
 "M-4" 'tim/tab-4
 "M-5" 'tim/tab-5
 "M-6" 'tim/tab-6
 "M-7" 'tim/tab-7
 "M-8" 'tim/tab-8
 "M-9" 'tim/tab-9

 ;; Tab navigation
 "M-TAB" 'tab-bar-switch-to-recent-tab     ; Next tab (M-Tab)
 "M-<tab>" 'tab-bar-switch-to-recent-tab   ; Alternative binding

 ;; Buffer navigation (bonus - not in tmux but useful)
 "M-h" 'previous-buffer                     ; Previous buffer
 "M-l" 'next-buffer)                        ; Next buffer

;; ========================================
;; Additional Useful Global Bindings
;; ========================================

(general-define-key
 :states '(normal visual insert emacs)

 ;; Make ESC quit prompts (already in evil.el but reinforce)
 "<escape>" 'keyboard-escape-quit

 ;; Quick save
 "C-s" 'save-buffer

 ;; Undo/Redo (more intuitive than Emacs defaults)
 "C-z" 'undo
 "C-S-z" 'undo-redo

 ;; Comment/uncomment
 "M-;" 'comment-line

 ;; Better beginning/end of line (like readline)
 "C-a" 'beginning-of-line-text
 "C-e" 'end-of-line)

;; ========================================
;; Mode-Specific Keybindings
;; ========================================

;; Dired mode enhancements
(with-eval-after-load 'dired
  (general-define-key
   :states 'normal
   :keymaps 'dired-mode-map
   "h" 'dired-up-directory
   "l" 'dired-find-file
   "o" 'dired-find-file-other-window
   "v" 'dired-view-file
   "q" 'quit-window))

;; Org-mode specific bindings (will be in docs.el too)
(with-eval-after-load 'org
  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   :prefix "SPC m"
   :prefix-command 'tim/org-mode-prefix
   "t" 'org-todo
   "d" 'org-deadline
   "s" 'org-schedule
   "a" 'org-archive-subtree
   "e" 'org-export-dispatch
   "l" 'org-insert-link))

;; ========================================
;; Function Keys (F1-F12)
;; ========================================

;; Some useful F-key bindings (optional, comment out if not desired)
(general-define-key
 "<f1>" 'help-command                       ; Help
 "<f2>" 'consult-buffer                     ; Quick buffer switch
 "<f3>" 'projectile-find-file               ; Quick file find
 "<f5>" 'revert-buffer                      ; Refresh buffer
 "<f8>" 'vterm-toggle                       ; Toggle terminal
 "<f9>" 'magit-status                       ; Git status
 "<f12>" 'eval-last-sexp)                   ; Eval lisp

;; ========================================
;; Which-Key Descriptions
;; ========================================

;; Add descriptions for our M-* bindings in which-key
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "M-o" "find file (project)"
    "M-d" "find file"
    "M-r" "ripgrep"
    "M-y" "file manager"
    "M-n" "new file"
    "M-c" "vterm"
    "M-C" "claude code"
    "M-g" "magit status"
    "M-G" "magit dispatch"
    "M-S" "switch project"
    "M-1" "tab 1"
    "M-2" "tab 2"
    "M-3" "tab 3"
    "M-4" "tab 4"
    "M-5" "tab 5"
    "M-6" "tab 6"
    "M-7" "tab 7"
    "M-8" "tab 8"
    "M-9" "tab 9"
    "M-TAB" "recent tab"))

;; ========================================
;; macOS-Specific Overrides
;; ========================================

(when (eq system-type 'darwin)
  ;; Override some macOS defaults that conflict
  (general-define-key
   "s-n" nil  ; Don't use Cmd-n for new frame
   "s-w" nil  ; Don't use Cmd-w for delete-frame
   "s-q" nil) ; Don't use Cmd-q for quit (use SPC q q instead)

  ;; Optionally bind Cmd key to common operations
  (general-define-key
   "s-s" 'save-buffer                       ; Cmd-s save
   "s-f" 'consult-line                      ; Cmd-f find in buffer
   "s-k" 'kill-current-buffer               ; Cmd-k kill buffer
   "s-t" 'tab-bar-new-tab))                 ; Cmd-t new tab

;;; keybindings.el ends here

;;; navigation.el --- Code navigation without LSP -*- lexical-binding: t; -*-
;;
;; Description: Simple code navigation with dumb-jump, xref, and etags
;;
;;; Commentary:
;; No LSP! Just grep-based navigation and tags.
;; You explicitly prefer: "Grepping, etags, and dumb jump are all we need"
;;
;; Tools configured:
;;   - dumb-jump: Jump to definition using ripgrep/ag/grep
;;   - xref: Built-in cross-reference system (works with etags/dumb-jump)
;;   - ctags: Integration with universal-ctags (you already use this)
;;   - wgrep: Edit grep results in-place
;;   - imenu: Navigate symbols in current buffer
;;
;;; Code:

;; ========================================
;; Dumb-Jump - Grep-Based Jump to Definition
;; ========================================

(use-package dumb-jump
  :config
  ;; Use ripgrep (you already use rg extensively)
  (setq dumb-jump-force-searcher 'rg)

  ;; Prefer external searchers
  (setq dumb-jump-prefer-searcher 'rg)

  ;; Show multiple results in a list (like FZF)
  (setq dumb-jump-selector 'completing-read) ; Works with vertico

  ;; Use project root for searches
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  ;; Aggressive search for better results
  (setq dumb-jump-aggressive t)

  ;; Quiet mode (less messages)
  (setq dumb-jump-quiet t))

;; ========================================
;; Xref - Built-in Cross-Reference
;; ========================================

(use-package xref
  :straight nil  ; Built-in
  :config
  ;; Use consult for xref results (better UI than default)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Save position before jumping
  (setq xref-prompt-for-identifier nil))

;; ========================================
;; Ctags Integration
;; ========================================

;; You already use universal-ctags extensively
;; This integrates ctags with xref for navigation

(defun tim/generate-tags ()
  "Generate ctags for current project."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (tags-file (expand-file-name "TAGS" project-root)))
    (message "Generating tags in %s..." project-root)
    (shell-command
     (format "cd %s && ctags -e -R --exclude=.git --exclude=node_modules --exclude=dist --exclude=build"
             (shell-quote-argument project-root)))
    (visit-tags-table tags-file)
    (message "Tags generated: %s" tags-file)))

(defun tim/update-tags-on-save ()
  "Update tags file when saving buffer in certain modes."
  (when (and (projectile-project-p)
             (derived-mode-p 'prog-mode))
    ;; Only regenerate occasionally (don't do it every save - too slow)
    ;; You can call tim/generate-tags manually when needed
    nil))

;; Set tags table search path
(setq tags-revert-without-query t)
(setq tags-case-fold-search nil)

;; ========================================
;; Imenu - Buffer Symbol Navigation
;; ========================================

(use-package imenu
  :straight nil  ; Built-in
  :config
  ;; Auto-rescan buffer for symbols
  (setq imenu-auto-rescan t)

  ;; Use flat index (no nesting)
  (setq imenu-use-markers t)

  ;; More precise indexing
  (setq imenu-auto-rescan-maxout (* 1024 1024))) ; 1MB

;; Enhanced imenu with consult (already configured in completion.el)
;; consult-imenu provides fuzzy search over buffer symbols

;; ========================================
;; Wgrep - Edit Grep Results
;; ========================================

(use-package wgrep
  :config
  ;; Make grep/ripgrep results editable
  (setq wgrep-auto-save-buffer t)  ; Auto-save after editing
  (setq wgrep-change-readonly-file nil))

;; ========================================
;; Avy - Jump to Visible Text
;; ========================================

;; Avy lets you jump to any visible text using character hints
;; Like vim-easymotion / vim-sneak
(use-package avy
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))

;; ========================================
;; Symbol Navigation Functions
;; ========================================

(defun tim/find-references ()
  "Find references to symbol at point using ripgrep."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (if symbol
        (if (projectile-project-p)
            (consult-ripgrep (projectile-project-root) symbol)
          (consult-ripgrep default-directory symbol))
      (message "No symbol at point"))))

(defun tim/find-definition ()
  "Find definition using xref (which uses dumb-jump/ctags)."
  (interactive)
  (call-interactively 'xref-find-definitions))

(defun tim/find-definition-other-window ()
  "Find definition in other window."
  (interactive)
  (call-interactively 'xref-find-definitions-other-window))

(defun tim/pop-marker ()
  "Pop back to previous location."
  (interactive)
  (xref-pop-marker-stack))

;; ========================================
;; Occur Mode Enhancement
;; ========================================

;; Occur is like grep for current buffer
(use-package replace
  :straight nil  ; Built-in (occur is part of replace)
  :config
  ;; Edit occur results with wgrep
  (add-hook 'occur-mode-hook 'wgrep-change-to-wgrep-mode))

;; ========================================
;; Keybindings
;; ========================================

(with-eval-after-load 'evil
  (with-eval-after-load 'general
    (when (fboundp 'tim/leader-keys)
      (tim/leader-keys
    ;; Code navigation prefix
    "c" '(:ignore t :which-key "code")

    ;; Jump to definition/references
    "cd" '(tim/find-definition :which-key "definition")
    "cD" '(tim/find-definition-other-window :which-key "definition (other window)")
    "cr" '(tim/find-references :which-key "references")
    "cb" '(xref-pop-marker-stack :which-key "back")

    ;; Symbol navigation
    "ci" '(consult-imenu :which-key "imenu")
    "cI" '(consult-imenu-multi :which-key "imenu (all buffers)")
    "cs" '(consult-outline :which-key "outline")

    ;; Tags
    "ct" '(:ignore t :which-key "tags")
    "ctg" '(tim/generate-tags :which-key "generate tags")
    "ctv" '(visit-tags-table :which-key "visit tags table")
    "ctf" '(xref-find-apropos :which-key "find tag")

    ;; Avy jumps
    "cj" '(:ignore t :which-key "jump")
    "cjj" '(avy-goto-char-timer :which-key "jump to char")
    "cjl" '(avy-goto-line :which-key "jump to line")
    "cjw" '(avy-goto-word-1 :which-key "jump to word")

    ;; Occur/Grep
    "co" '(occur :which-key "occur")
    "cg" '(consult-grep :which-key "grep")))))

;; Evil mode keybindings for navigation (mimic Vim)
(with-eval-after-load 'evil
  (general-define-key
   :states 'normal
   :keymaps 'override

   ;; Jump to definition (like Vim gd)
   "gd" 'tim/find-definition
   "gD" 'tim/find-definition-other-window

   ;; Jump to references (like Vim gr)
   "gr" 'tim/find-references

   ;; Pop back (like Vim C-o)
   "gb" 'xref-pop-marker-stack

   ;; Imenu (like Vim's buffer tag navigation)
   "gi" 'consult-imenu

   ;; Avy jumps (alternative to vim-sneak)
   "s" 'avy-goto-char-2))  ; Two-character jump

;; ========================================
;; Evil Integration for Xref
;; ========================================

;; Make xref buffers use evil keybindings
(with-eval-after-load 'xref
  (evil-set-initial-state 'xref--xref-buffer-mode 'normal))

;;; navigation.el ends here

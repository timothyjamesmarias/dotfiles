;;; docs.el --- Documentation and text editing -*- lexical-binding: t; -*-
;;
;; Description: Org-mode and Markdown configuration
;;
;;; Commentary:
;; Configuration for writing documentation, notes, and text editing.
;; Primary tools: org-mode (built-in) and markdown-mode.
;;
;; Org-mode is incredibly powerful for:
;;   - Note-taking and knowledge management
;;   - TODO lists and task tracking
;;   - Literate programming
;;   - Export to HTML, PDF, Markdown, etc.
;;
;;; Code:

;; ========================================
;; Org-Mode - The Killer App
;; ========================================

(use-package org
  :straight nil  ; Built-in
  :config
  ;; Directory for org files
  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "notes.org"))

  ;; Basic settings
  (setq org-startup-indented t)           ; Clean outline view
  (setq org-startup-folded 'content)      ; Fold by default
  (setq org-hide-emphasis-markers t)      ; Hide markup markers
  (setq org-pretty-entities t)            ; Show UTF-8 characters
  (setq org-use-sub-superscripts '{})     ; Require braces for sub/super

  ;; Better bullet points
  (setq org-list-allow-alphabetical t)
  (setq org-src-fontify-natively t)       ; Syntax highlighting in src blocks
  (setq org-src-tab-acts-natively t)      ; Tab in src blocks works natively

  ;; Code block settings
  (setq org-src-preserve-indentation t)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-content-indentation 0)

  ;; TODO keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; TODO keyword colors
  (setq org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("IN-PROGRESS" . "yellow")
          ("WAITING" . "orange")
          ("DONE" . "green")
          ("CANCELLED" . "red")))

  ;; Log when tasks are completed
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Archive settings
  (setq org-archive-location "archive/%s_archive::")

  ;; Agenda files (customize as needed)
  ;; (setq org-agenda-files '("~/org/"))

  ;; Export settings
  (setq org-export-with-toc t)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-smart-quotes t)
  (setq org-html-validation-link nil)

  ;; Enable speed keys (single-key commands at heading start)
  (setq org-use-speed-commands t))

;; ========================================
;; Org-Modern - Better Visual Appearance
;; ========================================

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star 'replace)
  (setq org-modern-table-vertical 1)
  (setq org-modern-table-horizontal 0.2)
  (setq org-modern-list '((42 . "◦") (43 . "•") (45 . "–"))))

;; ========================================
;; Org-Appear - Show Markup When Editing
;; ========================================

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t)
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t))

;; ========================================
;; Org-Babel - Code Execution in Org
;; ========================================

(with-eval-after-load 'org
  ;; Enable languages for code execution (add more as needed)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (js . t)
     (java . t)
     (ruby . t)))

  ;; Don't ask for confirmation before executing
  ;; WARNING: Only enable if you trust your org files!
  ;; (setq org-confirm-babel-evaluate nil)
  )

;; ========================================
;; Markdown-Mode
;; ========================================

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  ;; Use grip or marked for preview (requires external tool)
  ;; brew install grip
  (setq markdown-command "multimarkdown")

  ;; GitHub Flavored Markdown
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t)

  ;; Asymmetric header scaling
  (setq markdown-asymmetric-header t)

  ;; Open links in browser
  (setq markdown-open-command "open"))

;; Live markdown preview
(use-package markdown-preview-mode
  :after markdown-mode
  :config
  (setq markdown-preview-stylesheets
        (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css")))

;; ========================================
;; Evil Integration for Org/Markdown
;; ========================================

;; Org-mode evil integration (from evil-collection)
;; Already configured in evil.el, but add mode-specific tweaks

(with-eval-after-load 'evil-org
  ;; Use evil-org for better Vim keybindings in org-mode
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading)))))

;; Evil-org package for better org integration
(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; ========================================
;; Org-Roam - Zettelkasten Note System
;; ========================================

;; Uncomment if you want a zettelkasten-style note system
;; (use-package org-roam
;;   :after org
;;   :init
;;   (setq org-roam-directory (file-truename "~/org/roam/"))
;;   (setq org-roam-v2-ack t)
;;   :config
;;   (org-roam-db-autosync-mode)
;;   :general
;;   (tim/leader-keys
;;     "nr" '(:ignore t :which-key "roam")
;;     "nrf" '(org-roam-node-find :which-key "find node")
;;     "nri" '(org-roam-node-insert :which-key "insert node")
;;     "nrc" '(org-roam-capture :which-key "capture")
;;     "nrb" '(org-roam-buffer-toggle :which-key "buffer toggle")))

;; ========================================
;; Helper Functions
;; ========================================

(defun tim/org-insert-heading-below ()
  "Insert heading below current heading."
  (interactive)
  (org-insert-heading-after-current))

(defun tim/org-toggle-checkbox ()
  "Toggle checkbox at point."
  (interactive)
  (org-toggle-checkbox))

;; ========================================
;; Keybindings
;; ========================================

(with-eval-after-load 'evil
  (with-eval-after-load 'general
    (when (fboundp 'tim/leader-keys)
      (tim/leader-keys
    ;; Note operations (n prefix)
    "n" '(:ignore t :which-key "notes")
    "nf" '(find-file :which-key "find file")
    "nn" '((lambda () (interactive) (find-file org-default-notes-file)) :which-key "notes.org")

    ;; Org-specific (when in org-mode, use local leader SPC m)
    "no" '(:ignore t :which-key "org")
    "noa" '(org-agenda :which-key "agenda")
    "noc" '(org-capture :which-key "capture")
    "not" '(org-todo-list :which-key "todo list")
    "nol" '(org-store-link :which-key "store link")))))

;; Org-mode local leader keybindings (SPC m)
(with-eval-after-load 'org
  (with-eval-after-load 'general
    (when (fboundp 'tim/local-leader-keys)
      (tim/local-leader-keys
       :keymaps 'org-mode-map

       ;; Structure editing
       "h" '(org-insert-heading :which-key "insert heading")
       "H" '(org-insert-heading-after-current :which-key "insert heading below")
       "s" '(org-insert-subheading :which-key "insert subheading")

       ;; TODO operations
       "t" '(org-todo :which-key "todo")
       "T" '(org-todo-list :which-key "todo list")
       "d" '(org-deadline :which-key "deadline")
       "S" '(org-schedule :which-key "schedule")
       "x" '(org-toggle-checkbox :which-key "toggle checkbox")

       ;; Links
       "l" '(org-insert-link :which-key "insert link")
       "L" '(org-store-link :which-key "store link")

       ;; Archive/Refile
       "a" '(org-archive-subtree :which-key "archive")
       "r" '(org-refile :which-key "refile")

       ;; Export
       "e" '(org-export-dispatch :which-key "export")

       ;; Tables
       "i" '(:ignore t :which-key "insert")
       "it" '(org-table-create :which-key "table")

       ;; Code blocks
       "b" '(:ignore t :which-key "blocks")
       "bb" '(org-edit-src-code :which-key "edit src")
       "bc" '(org-babel-execute-src-block :which-key "execute block")
       "bt" '(org-babel-tangle :which-key "tangle")))))

;; ========================================
;; Auto-fill for Text Modes
;; ========================================

;; Enable auto-fill (word wrap) for text editing modes
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;; Set fill column (line width)
(setq-default fill-column 80)

;;; docs.el ends here

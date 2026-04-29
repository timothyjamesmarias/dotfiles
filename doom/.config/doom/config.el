;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; --- Settings ---
(setq org-directory "~/notes/")
(setq native-comp-deferred-compilation nil)
(setq display-line-numbers-type 'relative)
(setq server-name "doom")
(setq doom-theme 'doom-ir-black)
(setq doom-modeline-minor-modes t)
(setq doom-font (font-spec :family "MonoLisa" :size 14))
(setq-default line-spacing 4)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(global-display-line-numbers-mode)
(setq fancy-splash-image "~/.config/doom/emacs-e-logo.png")

;; --- Evil ---
(after! evil
  (define-key evil-insert-state-map (kbd "C-h") #'delete-backward-char)
  (define-key evil-ex-completion-map (kbd "C-h") #'delete-backward-char)
  (define-key evil-ex-search-keymap (kbd "C-h") #'delete-backward-char))

;; --- Magit ---
(after! magit
  (setq magit-pull-or-fetch 'pull)
  (setq magit-rebase-arguments '("--autostash"))
  (setq magit-pull-arguments '("--rebase" "--autostash")))

;; --- Vterm ---
(after! vterm
  (define-key vterm-mode-map (kbd "M-o") #'term-fast-toggle)

  (defun tim/vterm-dnd--paths (data)
    "Return a list of file path strings from a drag-n-drop event DATA."
    (cond
     ((stringp data) (list data))
     ((and (consp data) (symbolp (car data)) (stringp (cadr data)))
      (list (cadr data)))
     ((and (consp data) (consp (car data)))
      (mapcar (lambda (x) (if (consp x) (or (cdr-safe x) (cadr x)) x)) data))
     ((listp data)
      (seq-filter #'stringp data))))

  (defun tim/vterm-dnd (event)
    "Insert a dropped file's path at the vterm prompt instead of opening it."
    (interactive "e")
    (dolist (f (tim/vterm-dnd--paths (nth 2 event)))
      (let ((path (if (string-match "\\`file://" f)
                      (url-unhex-string (substring f (match-end 0)))
                    f)))
        (vterm-insert (concat (shell-quote-argument path) " ")))))

  (define-key vterm-mode-map [drag-n-drop] #'tim/vterm-dnd)
  (define-key vterm-mode-map [M-drag-n-drop] #'tim/vterm-dnd)
  (define-key vterm-mode-map [s-drag-n-drop] #'tim/vterm-dnd)

  (defun tim/with-editor-export-maybe ()
    "Export EDITOR into vterm unless this is a Claude Code buffer."
    (unless (string= (buffer-name) +tim/claude-buffer-name)
      (with-editor-export-editor)))

  (add-hook 'vterm-mode-hook #'tim/with-editor-export-maybe))

;; --- Tags ---
(defun +tim/tag-find-all ()
  "Pick between dumb-jump definition, rg usages, tags, and xref history."
  (interactive)
  (let* ((choices '(("Definition (lookup chain)" . +lookup/definition)
                    ("Usages (ripgrep)"          . +default/search-project-for-symbol-at-point)
                    ("All tags"                  . projectile-find-tag)
                    ("Jump history"              . xref-go-back)))
         (pick (completing-read "Find: " (mapcar #'car choices) nil t)))
    (call-interactively (cdr (assoc pick choices)))))

(map! :leader
      (:prefix ("t" . "tags")
       :desc "Definition at point" "d" #'+lookup/definition
       :desc "Usages (rg)"         "u" #'+default/search-project-for-symbol-at-point
       :desc "Browse all tags"     "g" #'projectile-find-tag
       :desc "Jump history"        "s" #'xref-go-back
       :desc "Find all"            "a" #'+tim/tag-find-all))

;; --- Writeroom ---
(after! writeroom-mode
  (setq +zen-text-scale 0
        writeroom-width 80
        writeroom-mode-line t
        visual-fill-column-center-text t)
  (add-hook 'writeroom-mode-enable-hook
            (defun +zen-enable-word-wrap-h ()
              (visual-line-mode +1)
              (+word-wrap-mode +1)))
  (add-hook 'writeroom-mode-disable-hook
            (defun +zen-disable-word-wrap-h ()
              (visual-line-mode -1)
              (+word-wrap-mode -1))))

;; --- Eglot (LSP) ---
;; Server overrides — must be outside (after! eglot) to register before first use.
;; set-eglot-client! handles deferred loading internally.
(set-eglot-client! '(kotlin-mode kotlin-ts-mode) '("kotlin-lsp" "--stdio"))
(set-eglot-client! '(php-mode php-ts-mode) '("intelephense" "--stdio"))

(after! eglot
  ;; Suppress chatty capabilities that clog the event loop.
  ;; Tree-sitter handles highlighting; custom handlers cover navigation gaps.
  (setq eglot-ignored-server-capabilities
        '(:inlayHintProvider              ; visual noise + CPU on every scroll
          :documentHighlightProvider       ; server round-trip on every cursor move
          :colorProvider                   ; redundant with Doom defaults
          :codeLensProvider                ; persistent overhead, rarely useful
          :semanticTokensProvider          ; tree-sitter handles highlighting
          :documentOnTypeFormattingProvider)) ; fires on every keystroke

  ;; Throttle change notifications — default 0.5s is aggressive during fast edits
  (setq eglot-send-changes-idle-time 0.75)

  ;; Don't extend eglot xref to non-managed buffers
  (setq eglot-extend-to-xref nil)

  ;; Single-line eldoc — reduce rendering cost on every cursor pause
  (setq eldoc-echo-area-use-multiline-p nil)

  (map! :leader
        (:prefix ("l" . "lsp")
         :desc "Rename"            "r" #'eglot-rename
         :desc "Code actions"      "a" #'eglot-code-actions
         :desc "Format buffer"     "f" #'eglot-format-buffer
         :desc "Format region"     "F" #'eglot-format
         :desc "Implementations"   "i" #'eglot-find-implementation
         :desc "Type definition"   "t" #'eglot-find-typeDefinition
         :desc "Workspace symbols" "s" #'consult-eglot-symbols
         :desc "Diagnostics"       "d" #'flymake-show-buffer-diagnostics
         :desc "Reconnect"         "R" #'eglot-reconnect
         :desc "Shutdown"          "q" #'eglot-shutdown)))

;; --- Projectile ---
(setq +workspaces-switch-project-function #'dired)

(after! projectile
  (advice-add #'projectile-visit-project-tags-table :override #'ignore))

(after! orderless
  (setq orderless-matching-styles
        '(orderless-literal orderless-regexp orderless-flex)))

;; --- Kotlin tree-sitter highlighting ---
;; The upstream kotlin-ts-mode has a broken string interpolation query
;; ("$"/"${"/"}" nodes don't exist in the grammar) that silently kills
;; fontification for everything after it.  We remove that rule and add
;; proper annotation, function-call, and property highlighting.
(after! kotlin-ts-mode
  (defvar +kotlin-ts--annotation-rules
    (treesit-font-lock-rules
     :language 'kotlin :feature 'annotation :override t
     '((annotation (user_type (type_identifier) @font-lock-preprocessor-face))
       (annotation (constructor_invocation
                    (user_type (type_identifier) @font-lock-preprocessor-face))))))

  (defvar +kotlin-ts--property-rules
    (treesit-font-lock-rules
     :language 'kotlin :feature 'property :override t
     '((navigation_expression
        (navigation_suffix (simple_identifier) @font-lock-property-use-face)))))

  (defvar +kotlin-ts--function-rules
    (treesit-font-lock-rules
     :language 'kotlin :feature 'function :override t
     '((call_expression (simple_identifier) @font-lock-function-call-face)
       (call_expression
        (navigation_expression
         (navigation_suffix
          (simple_identifier) @font-lock-function-call-face)))))))

(advice-add 'kotlin-ts-mode :after
            (defun +kotlin-ts-mode--enhance-h (&rest _)
              "Fix broken upstream rules and add richer highlighting."
              (when (boundp '+kotlin-ts--annotation-rules)
                ;; Remove rule 3 (broken string interp query) from upstream
                (setq-local treesit-font-lock-settings
                            (append (cl-subseq treesit-font-lock-settings 0 3)
                                    (cl-subseq treesit-font-lock-settings 4)
                                    +kotlin-ts--annotation-rules
                                    +kotlin-ts--property-rules
                                    +kotlin-ts--function-rules))
                (setq-local treesit-font-lock-feature-list
                            '((comment number string definition)
                              (keyword builtin type constant variable)
                              (escape-sequence function property)
                              (annotation)))
                (treesit-font-lock-recompute-features))))

;; --- Custom modules ---
(load! "modules/buffers")
(load! "modules/docker")
(load! "modules/files")
(load! "modules/github")
(load! "modules/macos")
(load! "modules/notes")
(load! "modules/framework-detect")
(load! "modules/maizzle")
(load! "modules/rails-nav")
(load! "modules/claude")
(load! "modules/database")

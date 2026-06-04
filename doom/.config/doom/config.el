;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; --- Settings ---
(setq org-directory "~/notes/")
(setq native-comp-jit-compilation nil)
;; Make `doom/reload' AOT native-compile changed packages (incremental).
(setq doom-reload-command
      (if (featurep :system 'android)
          "sh %s sync -B -e --aot"
        "%s sync -B -e --aot"))
(after! evil-escape
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.15))
(setq display-line-numbers-type 'relative)
(setq server-name "doom")
;; everforest contrast: "hard" | "medium" | "soft" (default soft)
;; palette: "material" | "mix" | "original" (default material)
;; These must be set BEFORE the theme loads, since the theme builds its
;; palette at load time.
(setq doom-everforest-background "medium"
      doom-everforest-palette "material")
;; doom-everforest is installed via `package!' (straight), so its build dir is
;; on `load-path' but NOT on `custom-theme-load-path' — register it so
;; `load-theme' can find the theme file.
(when-let ((lib (locate-library "doom-everforest-theme")))
  (add-to-list 'custom-theme-load-path (file-name-directory lib)))
(setq doom-theme 'doom-everforest)
;; --- Punch up tree-sitter (treesit) highlighting under Everforest -----------
;; doom-themes-base mutes these secondary faces by blending toward fg, which
;; looks flat on Everforest's soft palette. Map them to full palette colors.
;; `custom-set-faces!' re-applies automatically when the theme (re)loads.
(custom-set-faces!
  `(font-lock-function-call-face   :foreground ,(doom-color 'teal)   :slant normal)
  `(font-lock-variable-use-face    :foreground ,(doom-color 'blue))
  `(font-lock-property-use-face    :foreground ,(doom-color 'yellow))
  `(font-lock-property-name-face   :foreground ,(doom-color 'yellow) :weight normal)
  `(font-lock-operator-face        :foreground ,(doom-color 'orange))
  `(font-lock-bracket-face         :foreground ,(doom-color 'fg))
  `(font-lock-delimiter-face       :foreground ,(doom-color 'base7)))
(setq doom-modeline-minor-modes t)
(setq doom-font (font-spec :family "MonoLisa" :size 14))
(setq-default line-spacing 0.3)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(global-display-line-numbers-mode)
(defun my-custom-banner ()
  (let ((banner-file (expand-file-name "~/.config/doom/splash.txt")))
    (propertize
     (with-temp-buffer
       (insert-file-contents banner-file)
       (buffer-string))
     'face '+dashboard-banner)))
(setq +dashboard-ascii-banner-fn #'my-custom-banner)
(setq treemacs-width 50)

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

;; --- Blamer (inline git blame, JetBrains GitToolBox-style) ---
(use-package! blamer
  :defer 1
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 40)
  (blamer-author-formatter " ✎ %s ")
  (blamer-datetime-formatter "[%s] ")
  (blamer-commit-formatter "● %s")
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 0.9
                   :italic t)))
  :config
  (global-blamer-mode 1))

;; --- Vterm ---
(after! vterm
  (define-key vterm-mode-map (kbd "M-o") #'term-fast-toggle)
  (evil-define-key 'emacs vterm-mode-map (kbd "C-g") #'evil-normal-state)

  (defun tim/vterm-jk-escape ()
    "In vterm emacs-state, intercept `j` and switch to normal state if `k` follows."
    (interactive)
    (vterm-send-string "j")
    (let ((evt (read-event nil nil 0.15)))
      (cond
       ((and evt (char-equal evt ?k))
        (vterm-send-key "<backspace>")
        (evil-normal-state))
       (evt (push (cons t evt) unread-command-events))))) ;; re-queue non-k key

  (evil-define-key 'emacs vterm-mode-map (kbd "j") #'tim/vterm-jk-escape)

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
(set-eglot-client! '(elixir-mode elixir-ts-mode heex-ts-mode) '("elixir-ls-wrapper"))

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
  (setq treemacs-follow-mode t)
  (advice-add #'projectile-visit-project-tags-table :override #'ignore)
  ;; Use `rg --files` so projectile honors .gitignore AND .rgignore (and
  ;; .ignore) in one pass. Falls back to projectile's default if rg is missing.
  (when (executable-find "rg")
    (setq projectile-indexing-method 'alien
          ;; Projectile prefers fd via `projectile-git-use-fd' when available,
          ;; which would bypass `projectile-git-command' (and our .rgignore).
          projectile-git-use-fd nil
          projectile-git-command "rg --files -0 --hidden --glob '!.git'"
          projectile-git-submodule-command nil)))

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

;; --- Mermaid ---
(use-package! ob-mermaid
  :defer t
  :config
  (setq ob-mermaid-cli-path (executable-find "mmdc")
        ob-mermaid-default-config-file
        (expand-file-name "mermaid-config.json" doom-user-dir)))

(after! org
  (add-hook 'org-babel-after-execute-hook
            (defun +mermaid-redisplay-images-h ()
              (when (org-in-src-block-p)
                (org-redisplay-inline-images)))))

(use-package! mermaid-mode
  :defer t
  :mode "\\.mmd\\'")

(defun +mermaid/preview ()
  "Render the mermaid diagram at point in a side buffer.
Works from markdown fenced blocks, org src blocks, .mmd files, or region."
  (interactive)
  (let* ((src (or (+mermaid--extract-fenced-block)
                  (+mermaid--extract-org-block)
                  (when (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))
                  (when (string-suffix-p ".mmd" (or (buffer-file-name) ""))
                    (buffer-substring-no-properties (point-min) (point-max)))))
         (mmdc (or (executable-find "mmdc")
                   (user-error "mmdc not found — install @mermaid-js/mermaid-cli")))
         (infile (make-temp-file "mermaid-" nil ".mmd"))
         (outfile (concat (file-name-sans-extension infile) ".svg"))
         (config (expand-file-name "mermaid-config.json" doom-user-dir)))
    (unless src (user-error "No mermaid diagram found at point"))
    (with-temp-file infile (insert src))
    (let ((exit (call-process mmdc nil "*mermaid-errors*" nil
                              "-i" infile "-o" outfile
                              "-c" config)))
      (unless (and (zerop exit) (file-exists-p outfile))
        (pop-to-buffer "*mermaid-errors*")
        (user-error "mmdc failed (exit %d)" exit)))
    (let ((buf (get-buffer-create "*mermaid-preview*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-image (create-image outfile 'svg nil
                                      :max-width (/ (frame-pixel-width) 2)))
          (goto-char (point-min))
          (special-mode)))
      (display-buffer buf '(display-buffer-in-side-window
                            (side . right) (window-width . 0.45))))))

(defun +mermaid--extract-fenced-block ()
  "Extract mermaid source from a markdown fenced block at point."
  (save-excursion
    (let ((pos (point)))
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward "^```mermaid\\s-*$" nil t)
          (let ((start (line-beginning-position 2)))
            (when (re-search-forward "^```\\s-*$" nil t)
              (let ((end (line-beginning-position)))
                (when (and (<= start pos) (<= pos (line-end-position)))
                  (throw 'found (buffer-substring-no-properties start end)))))))))))

(defun +mermaid--extract-org-block ()
  "Extract mermaid source from an org src block at point."
  (when (and (derived-mode-p 'org-mode) (org-in-src-block-p))
    (let ((info (org-babel-get-src-block-info 'light)))
      (when (string= (car info) "mermaid")
        (nth 1 info)))))

(map! :leader
      (:prefix ("M" . "mermaid")
       :desc "Preview diagram" "p" #'+mermaid/preview))

;; --- Clojure / CIDER ---
;; Override Doom's +clojure/open-repl which uses a blocking while-loop
;; that freezes Emacs during nREPL connection.  Let CIDER handle it
;; asynchronously — it already pops the REPL buffer on connect.
(after! cider
  (defun +clojure/open-repl (&optional arg type)
    "Open a Cider REPL without blocking Emacs."
    (interactive "P")
    (let ((type (or type 'clj)))
      (if-let* ((buffer (cider-current-repl type)))
          (pop-to-buffer buffer)
        (if (eq type 'clj)
            (cider-jack-in-clj arg)
          (cider-jack-in-cljs arg))))))

;; --- Structural Editing (evil-cleverparens) ---
(use-package! evil-cleverparens
  :hook ((emacs-lisp-mode . evil-cleverparens-mode)
         (clojure-mode . evil-cleverparens-mode)
         (clojurescript-mode . evil-cleverparens-mode)
         (lisp-mode . evil-cleverparens-mode))
  :config
  (setq evil-cleverparens-use-s-and-S nil))

;; --- Custom modules ---
(load! "modules/buffers")
(load! "modules/docker")
(load! "modules/files")
(load! "modules/ripgrep")
(load! "modules/github")
(load! "modules/macos")
(load! "modules/notes")
(load! "modules/dwim-nav")
(load! "modules/framework-detect")
(load! "modules/maizzle")
(load! "modules/rails-nav")
(load! "modules/laravel-nav")
;; (load! "modules/ruby-etags")  ; disabled while testing dwim-nav
(load! "modules/claude")
(load! "modules/database")
(load! "modules/ebooks")
(load! "modules/elfeed")
(load! "modules/magit")

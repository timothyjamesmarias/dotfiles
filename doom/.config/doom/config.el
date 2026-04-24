;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq display-line-numbers-type t)
(setq org-directory "~/notes/")
(setq native-comp-deferred-compilation nil)
(after! evil
  (define-key evil-insert-state-map (kbd "C-h") #'delete-backward-char)
  (define-key evil-ex-completion-map (kbd "C-h") #'delete-backward-char)
  (define-key evil-ex-search-keymap (kbd "C-h") #'delete-backward-char))

(after! magit
  (setq magit-pull-or-fetch 'pull)
  (setq magit-rebase-arguments '("--autostash"))
  (setq magit-pull-arguments '("--rebase" "--autostash")))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(setq doom-theme 'doom-monokai-spectrum)
(setq doom-font (font-spec :family "MonoLisa" :size 14))
(setq-default line-spacing 4)

(defun new-note ()
  "Create a new note from template in org-directory."
  (interactive)
  (let* ((title (read-string "Note title: "))
         (slug (replace-regexp-in-string
                "[^a-z0-9]+" "-"
                (downcase title)))
         (slug (replace-regexp-in-string
                "^-\\|-$" "" slug))
         (filename (expand-file-name
                    (concat slug ".org") org-directory))
         (date (format-time-string "%Y-%m-%d")))
    (when (file-exists-p filename)
      (unless (y-or-n-p (format "%s exists. Open it? " filename))
        (user-error "Aborted")))
    (find-file filename)
    (when (= (buffer-size) 0)
      (insert (format "#+title: %s\n#+date: %s\n\n* Content\n" title date))
      (save-buffer))))

(after! vterm
  (define-key vterm-mode-map (kbd "M-o") #'term-fast-toggle))

(map! :leader
      (:prefix ("n" . "notes")
       :desc "New note" "w" #'new-note))

(defun +tim/project-workspace-buffers ()
  "Buffers that belong to both the current project and current workspace.
Falls back gracefully when one or both scopes are unavailable."
  (let* ((in-project (and (fboundp 'projectile-project-p)
                          (projectile-project-p)))
         (proj (and in-project (projectile-project-buffers)))
         (persp (and (bound-and-true-p persp-mode)
                     (fboundp 'persp-buffer-list)
                     (persp-buffer-list))))
    (cond
     ((and proj persp) (seq-intersection proj persp))
     (proj proj)
     (persp persp)
     (t (buffer-list)))))

(defun +tim/cycle-buffer (direction)
  "Cycle to the next (1) or previous (-1) project×workspace buffer."
  (let* ((bufs (+tim/project-workspace-buffers))
         (bufs (seq-filter
                (lambda (b)
                  (and (not (string-prefix-p " " (buffer-name b)))
                       (not (with-current-buffer b
                              (derived-mode-p 'vterm-mode
                                              'term-mode
                                              'eshell-mode
                                              'shell-mode
                                              'comint-mode
                                              'magit-mode)))
                       (if (fboundp 'doom-real-buffer-p)
                           (doom-real-buffer-p b)
                         t)))
                bufs))
         (bufs (sort bufs (lambda (a b)
                            (string< (buffer-name a) (buffer-name b))))))
    (if (or (null bufs) (<= (length bufs) 1))
        (if (> direction 0) (next-buffer) (previous-buffer))
      (let* ((cur (current-buffer))
             (idx (or (seq-position bufs cur) 0))
             (nxt (mod (+ idx direction) (length bufs))))
        (switch-to-buffer (nth nxt bufs))))))

(defun +tim/next-project-workspace-buffer ()
  "Cycle to the next buffer in the current project ∩ workspace."
  (interactive) (+tim/cycle-buffer 1))

(defun +tim/previous-project-workspace-buffer ()
  "Cycle to the previous buffer in the current project ∩ workspace."
  (interactive) (+tim/cycle-buffer -1))

(map! :n "C-n" #'+tim/next-project-workspace-buffer
      :n "C-p" #'+tim/previous-project-workspace-buffer)

(defun +tim/maizzle-component-at-point ()
  "Return filesystem path for a Maizzle/Laravel component tag at point, or nil."
  (save-excursion
    (let* ((line (thing-at-point 'line t))
           (root (or (doom-project-root) default-directory)))
      (when line
        (cond
         ((string-match "</?x-\\([[:alnum:]._-]+\\)" line)
          (let* ((name (match-string 1 line))
                 (dotted (split-string name "\\.")))
            (cl-loop for candidate in
                     (append
                      (when (> (length dotted) 1)
                        (list (format "components/%s/%s.html"
                                      (mapconcat #'identity (butlast dotted) "/")
                                      (car (last dotted)))))
                      (list (format "components/%s.html" name)
                            (format "components/%s/index.html" name)
                            (format "resources/views/components/%s.blade.php"
                                    (replace-regexp-in-string "\\." "/" name))))
                     for abs = (expand-file-name candidate root)
                     when (file-readable-p abs) return abs)))
         ((string-match "<component[[:space:]]+src=\"\\([^\"]+\\)\"" line)
          (let ((abs (expand-file-name (match-string 1 line) root)))
            (when (file-readable-p abs) abs))))))))

(defun +tim/maizzle-lookup-file-handler (&optional _identifier)
  "Lookup handler for `+lookup/file' in Maizzle/Blade buffers."
  (when-let ((path (+tim/maizzle-component-at-point)))
    (find-file path)
    t))

(set-lookup-handlers! '(web-mode html-mode mhtml-mode php-mode)
  :file #'+tim/maizzle-lookup-file-handler)

(defun +tim/maizzle-find-references ()
  "Grep project for `<x-NAME>` usages of the current Maizzle component."
  (interactive)
  (let* ((file (buffer-file-name))
         (rel  (and file (file-relative-name file (doom-project-root))))
         (name (and rel (string-match "components/\\(.+\\)\\.html\\'" rel)
                    (match-string 1 rel))))
    (unless name (user-error "Not in components/*.html"))
    (setq name
          (cond ((string-match "\\(.+\\)/index\\'" name) (match-string 1 name))
                ((string-match "\\(.+\\)/\\(.+\\)\\'" name)
                 (concat (match-string 1 name) "." (match-string 2 name)))
                (t name)))
    (consult-ripgrep (doom-project-root) (format "<x-%s" name))))

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

(defun +tim/--gh-project-root ()
  "Return project root or signal a user error."
  (or (and (fboundp 'doom-project-root) (doom-project-root))
      (locate-dominating-file default-directory ".git")
      (user-error "Not inside a git project")))

(defun +tim/--gh-require ()
  "Ensure the `gh' CLI is available on PATH."
  (unless (executable-find "gh")
    (user-error "`gh' CLI not found on PATH")))

(defun +tim/gh-pr-create (&optional arg)
  "Create a GitHub pull request for the current branch.
Default runs `gh pr create --fill --web' to open the PR draft in the
browser with title/body pre-filled from commits. With prefix ARG, run
`gh pr create' interactively in a compile buffer."
  (interactive "P")
  (+tim/--gh-require)
  (let ((default-directory (+tim/--gh-project-root)))
    (if arg
        (compile "gh pr create" t)
      (let ((buf (get-buffer-create "*gh pr create*")))
        (with-current-buffer buf (erase-buffer))
        (let ((code (call-process "gh" nil buf nil
                                  "pr" "create" "--fill" "--web")))
          (if (zerop code)
              (message "gh pr create: opened in browser")
            (pop-to-buffer buf)
            (user-error "gh pr create failed (exit %d)" code)))))))

(defun +tim/gh-browse ()
  "Open the current repo on GitHub. If visiting a file inside the repo,
open that file at the current line."
  (interactive)
  (+tim/--gh-require)
  (let* ((root (+tim/--gh-project-root))
         (default-directory root)
         (file (buffer-file-name))
         (target (when (and file (file-in-directory-p file root))
                   (format "%s:%d"
                           (file-relative-name file root)
                           (line-number-at-pos))))
         (args (if target (list "browse" target) (list "browse")))
         (code (apply #'call-process "gh" nil nil nil args)))
    (unless (zerop code)
      (user-error "gh browse failed (exit %d)" code))))

(defun +tim/gh-pr-view ()
  "Open the pull request for the current branch in the browser."
  (interactive)
  (+tim/--gh-require)
  (let* ((default-directory (+tim/--gh-project-root))
         (code (call-process "gh" nil nil nil "pr" "view" "--web")))
    (unless (zerop code)
      (user-error "gh pr view failed (exit %d) — is there a PR for this branch?" code))))

(map! :leader
      (:prefix ("g h" . "github")
       :desc "Create PR"            "p" #'+tim/gh-pr-create
       :desc "Open repo in browser" "o" #'+tim/gh-browse
       :desc "View PR in browser"   "v" #'+tim/gh-pr-view))

(defun +tim/--macos-require ()
  "Ensure we're on macOS with the `open' CLI available."
  (unless (eq system-type 'darwin)
    (user-error "macOS only"))
  (unless (executable-find "open")
    (user-error "`open' CLI not found on PATH")))

(defun +tim/--macos-list-apps ()
  "Return a list of installed .app bundle names for completion."
  (let ((dirs '("/Applications" "/System/Applications"
                "/Applications/Utilities" "/System/Applications/Utilities"))
        names)
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (f (directory-files dir nil "\\.app\\'" t))
          (push (file-name-sans-extension f) names))))
    (sort (delete-dups names) #'string<)))

(defun +tim/macos-open-with (app)
  "Open the current file (or dired file at point) with APP via `open -a'."
  (interactive
   (list (completing-read "Open with app: " (+tim/--macos-list-apps) nil nil)))
  (+tim/--macos-require)
  (+macos-open-with app))

(map! :leader
      (:prefix "o"
       :desc "Open in default app" "x" #'+macos/open-in-default-program
       :desc "Open with app…"      "w" #'+tim/macos-open-with))

(defun +tim/slugify-string (str)
  "Slugify STR: downcase, strip non-alphanumeric to hyphens, trim edges."
  (let* ((s (downcase str))
         (s (replace-regexp-in-string "[^a-z0-9]+" "-" s))
         (s (replace-regexp-in-string "\\`-+\\|-+\\'" "" s)))
    s))

(defun +tim/slugify-rename-file ()
  "Rename the current buffer's file to its slugified form."
  (interactive)
  (let* ((file (or (buffer-file-name)
                   (user-error "Buffer is not visiting a file")))
         (dir (file-name-directory file))
         (base (file-name-sans-extension (file-name-nondirectory file)))
         (ext (file-name-extension file t))
         (slug (+tim/slugify-string base))
         (new (expand-file-name (concat slug ext) dir)))
    (when (string= file new)
      (user-error "Filename is already slugified"))
    (when (file-exists-p new)
      (user-error "Target already exists: %s" new))
    (when (y-or-n-p (format "Rename to %s?" (file-name-nondirectory new)))
      (if (vc-backend file)
          (vc-rename-file file new)
        (rename-file file new t)
        (set-visited-file-name new t t)))))

(defun +tim/dired-slugify-files ()
  "Slugify marked files in dired (or file at point)."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (dolist (file files)
      (let* ((dir (file-name-directory file))
             (base (file-name-sans-extension (file-name-nondirectory file)))
             (ext (file-name-extension file t))
             (slug (+tim/slugify-string base))
             (new (expand-file-name (concat slug ext) dir)))
        (unless (string= file new)
          (if (file-exists-p new)
              (message "Skipping %s — target exists" (file-name-nondirectory new))
            (if (vc-backend file)
                (vc-rename-file file new)
              (rename-file file new t))))))
    (revert-buffer)))

(map! :leader
      (:prefix "f"
       :desc "Slugify filename" "-" #'+tim/slugify-rename-file))

(after! dired
  (map! :map dirvish-mode-map
        :n "gs" #'+tim/dired-slugify-files))

;;; --- New File (template-based file creation) ---

(defvar +tim/templates-dir
  (expand-file-name "~/dotfiles/templates/")
  "Directory containing file templates and templates.conf.")

(defvar +tim/templates-conf nil
  "Parsed template configuration. Populated lazily from templates.conf.")

(defun +tim/templates--parse-conf ()
  "Parse templates.conf and cache the result."
  (or +tim/templates-conf
      (let ((conf-file (expand-file-name "templates.conf" +tim/templates-dir))
            entries)
        (when (file-readable-p conf-file)
          (with-temp-buffer
            (insert-file-contents conf-file)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (unless (or (string-empty-p line)
                            (string-prefix-p "#" line))
                  (let ((fields (split-string line "|")))
                    (when (= (length fields) 5)
                      (push (list :lang (nth 0 fields)
                                  :ext (nth 1 fields)
                                  :kind (nth 2 fields)
                                  :path (nth 3 fields)
                                  :desc (nth 4 fields))
                            entries)))))
              (forward-line 1))))
        (setq +tim/templates-conf (nreverse entries)))))

(defun +tim/templates--languages ()
  "Return sorted unique language names."
  (seq-uniq (mapcar (lambda (e) (plist-get e :lang))
                    (+tim/templates--parse-conf))))

(defun +tim/templates--kinds (lang)
  "Return available kinds for LANG as alist of (kind . description)."
  (let (kinds)
    (dolist (e (+tim/templates--parse-conf))
      (when (string= (plist-get e :lang) lang)
        (push (cons (plist-get e :kind) (plist-get e :desc)) kinds)))
    (nreverse kinds)))

(defun +tim/templates--get (lang kind)
  "Return the template entry for LANG and KIND."
  (seq-find (lambda (e)
              (and (string= (plist-get e :lang) lang)
                   (string= (plist-get e :kind) kind)))
            (+tim/templates--parse-conf)))

(defun +tim/templates--to-pascal (str)
  "Convert STR to PascalCase."
  (mapconcat (lambda (word) (concat (upcase (substring word 0 1))
                                    (substring word 1)))
             (split-string str "[-_]+" t) ""))

(defun +tim/templates--to-kebab (str)
  "Convert STR to kebab-case."
  (downcase
   (replace-regexp-in-string
    "\\`-+\\|-+\\'" ""
    (replace-regexp-in-string
     "[A-Z]" (lambda (m) (concat "-" (downcase m)))
     str))))

(defun +tim/templates--namespace (filepath lang)
  "Infer namespace/package from FILEPATH for LANG."
  (let ((dir (directory-file-name
              (file-name-directory (or filepath "")))))
    (pcase lang
      ((or "java" "kotlin" "scala")
       (let ((stripped dir))
         (dolist (root '("src/main/java/" "src/main/kotlin/" "src/main/scala/"
                         "src/test/java/" "src/test/kotlin/" "src/test/scala/"
                         "src/"))
           (when (string-match (regexp-quote root) stripped)
             (setq stripped (substring stripped (match-end 0)))))
         (if (or (string-empty-p stripped) (string= stripped "."))
             ""
           (replace-regexp-in-string "/" "." stripped))))
      ("php"
       (let ((stripped dir))
         (when (string-match "src/" stripped)
           (setq stripped (substring stripped (match-end 0))))
         (if (or (string-empty-p stripped) (string= stripped "."))
             ""
           (replace-regexp-in-string "/" "\\\\" stripped))))
      (_ ""))))

(defun +tim/templates--expand-placeholders (content filepath lang)
  "Replace {{PLACEHOLDER}} tokens in CONTENT for FILEPATH."
  (let* ((filename (file-name-sans-extension
                    (file-name-nondirectory filepath)))
         (class (+tim/templates--to-pascal filename))
         (component (+tim/templates--to-kebab filename))
         (namespace (+tim/templates--namespace filepath lang))
         (date (format-time-string "%Y-%m-%d")))
    (dolist (pair `(("{{DATE}}" . ,date)
                    ("{{CLASS_NAME}}" . ,class)
                    ("{{INTERFACE_NAME}}" . ,class)
                    ("{{MODULE_NAME}}" . ,filename)
                    ("{{COMPONENT_NAME}}" . ,component)
                    ("{{FILE_NAME}}" . ,filename)
                    ("{{NAMESPACE}}" . ,namespace)
                    ("{{TITLE}}" . ,class)
                    ("{{SELECTOR}}" . ,component)
                    ("{{HEADER_GUARD}}" . ,(upcase (replace-regexp-in-string
                                                    "-" "_" filename)))))
      (setq content (replace-regexp-in-string
                     (regexp-quote (car pair)) (cdr pair) content t t)))
    content))

(defun +tim/new-file ()
  "Create a new file from a template.
Interactive flow: pick language → pick kind → enter filename → create."
  (interactive)
  (let* ((conf (+tim/templates--parse-conf))
         (_ (unless conf (user-error "No templates found in %s" +tim/templates-dir)))
         ;; 1. Language
         (lang (completing-read "Language: " (+tim/templates--languages) nil t))
         ;; 2. Kind
         (kinds (+tim/templates--kinds lang))
         (kind (if (= (length kinds) 1)
                   (caar kinds)
                 (completing-read (format "Kind (%s): " lang)
                                  (mapcar (lambda (k) (format "%s — %s" (car k) (cdr k)))
                                          kinds)
                                  nil t)))
         (kind (car (split-string kind " — ")))
         ;; 3. Get template entry
         (entry (+tim/templates--get lang kind))
         (ext (plist-get entry :ext))
         ;; 4. Target directory
         (dir (or (and (fboundp 'doom-project-root) (doom-project-root))
                  default-directory))
         ;; 5. Filename
         (input (read-string (format "File name (.%s): " ext)))
         (input (if (string-suffix-p (concat "." ext) input)
                    input
                  (concat input "." ext)))
         (filepath (expand-file-name input dir)))
    ;; Check for existing file
    (when (file-exists-p filepath)
      (user-error "File already exists: %s" filepath))
    ;; Load and expand template
    (let* ((template-path (expand-file-name (plist-get entry :path) +tim/templates-dir))
           (content (if (file-readable-p template-path)
                        (with-temp-buffer
                          (insert-file-contents template-path)
                          (buffer-string))
                      ""))
           (expanded (+tim/templates--expand-placeholders content filepath lang)))
      ;; Create directories if needed
      (make-directory (file-name-directory filepath) t)
      ;; Create the file
      (find-file filepath)
      (insert expanded)
      (goto-char (point-min))
      ;; Jump to first TODO or end of buffer
      (unless (re-search-forward "TODO" nil t)
        (goto-char (point-max)))
      (save-buffer)
      (message "Created %s" (file-relative-name filepath dir)))))

(map! :leader
      (:prefix "f"
       :desc "New file from template" "n" #'+tim/new-file))

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

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq display-line-numbers-type t)
(setq org-directory "~/notes/")
(setq native-comp-deferred-compilation nil)

(after! magit
  (setq magit-pull-or-fetch 'pull)
  (setq magit-rebase-arguments '("--autostash"))
  (setq magit-pull-arguments '("--rebase" "--autostash")))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(setq doom-theme 'doom-material-dark)
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
                bufs)))
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

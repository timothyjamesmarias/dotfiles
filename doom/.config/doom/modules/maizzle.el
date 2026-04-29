;;; modules/maizzle.el --- Maizzle/Blade component navigation -*- lexical-binding: t; -*-

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

;; :file must be registered manually (Doom bug: make-list 5 truncates cl-mapc,
;; skipping :file and :xref-backend handlers entirely).
(defun +tim/maizzle-nav-mode-setup-h ()
  "Register :file handler for gf on Maizzle component tags."
  (if +tim/maizzle-nav-mode
      (add-hook '+lookup-file-functions #'+tim/maizzle-lookup-file-handler nil 'local)
    (remove-hook '+lookup-file-functions #'+tim/maizzle-lookup-file-handler 'local)))

(add-hook '+tim/maizzle-nav-mode-hook #'+tim/maizzle-nav-mode-setup-h)

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

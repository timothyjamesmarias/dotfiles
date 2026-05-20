;;; modules/ripgrep.el --- rginit: scaffold .rgignore/.fdignore from templates -*- lexical-binding: t; -*-

(defvar +tim/rgignore-templates-dir
  (expand-file-name "~/dotfiles/ripgrep/templates/")
  "Directory containing *.rgignore templates.")

(defvar +tim/rgignore-default-theme-ignore-patterns
  '("^twenty" "^genesis-block-theme$" "^Divi$")
  "Regexps matching theme directory names that should be treated as defaults.
Used when proposing candidate custom themes from `wp-content/themes/`.")

(defun +tim/rgignore--templates ()
  "Return alist of (NAME . PATH) for available rgignore templates."
  (when (file-directory-p +tim/rgignore-templates-dir)
    (mapcar (lambda (f)
              (cons (file-name-base f) f))
            (directory-files +tim/rgignore-templates-dir t "\\.rgignore\\'"))))

(defun +tim/rgignore--candidate-themes (dir)
  "Return list of likely-custom theme names under DIR/wp-content/themes/."
  (let ((themes-dir (expand-file-name "wp-content/themes/" dir)))
    (when (file-directory-p themes-dir)
      (let ((all (directory-files themes-dir nil "\\`[^.]" t)))
        (seq-filter
         (lambda (name)
           (and (file-directory-p (expand-file-name name themes-dir))
                (not (seq-some (lambda (rx) (string-match-p rx name))
                               +tim/rgignore-default-theme-ignore-patterns))))
         all)))))

(defun +tim/rgignore--expand-themes (themes)
  "Build the {{THEMES}} block: un-ignore THEMES, re-ignore their build dirs."
  (mapconcat
   (lambda (theme)
     (string-join
      (list (format "!wp-content/themes/%s/" theme)
            (format "wp-content/themes/%s/node_modules/" theme)
            (format "wp-content/themes/%s/vendor/" theme))
      "\n"))
   themes
   "\n"))

(defun +tim/rgignore--read-themes (dir)
  "Prompt for a list of WordPress themes to keep, defaulting to detected ones."
  (let* ((candidates (+tim/rgignore--candidate-themes dir))
         (prompt (if candidates
                     (format "Themes to keep (comma-separated) [%s]: "
                             (string-join candidates ","))
                   "Themes to keep (comma-separated): "))
         (selected (completing-read-multiple prompt candidates nil nil)))
    (or selected candidates)))

(defun +tim/rgignore--expand (content dir)
  "Replace {{PLACEHOLDER}} tokens in CONTENT, prompting as needed."
  (when (string-match-p "{{THEMES}}" content)
    (let ((themes (+tim/rgignore--read-themes dir)))
      (setq content (replace-regexp-in-string
                     "{{THEMES}}"
                     (+tim/rgignore--expand-themes themes)
                     content t t))))
  content)

(defun +tim/rginit (&optional template)
  "Create .rgignore and .fdignore in the current project from TEMPLATE.
Interactively, prompt for a template with completion. Templates may contain
{{THEMES}} placeholders, which are resolved by prompting."
  (interactive)
  (let* ((templates (+tim/rgignore--templates))
         (_ (unless templates
              (user-error "No templates found in %s" +tim/rgignore-templates-dir)))
         (name (or template
                   (completing-read "rgignore template: "
                                    (mapcar #'car templates) nil t)))
         (template-file (cdr (assoc name templates)))
         (_ (unless template-file
              (user-error "Unknown template: %s" name)))
         (dir (or (and (fboundp 'doom-project-root) (doom-project-root))
                  default-directory))
         (rgignore (expand-file-name ".rgignore" dir))
         (fdignore (expand-file-name ".fdignore" dir)))
    (when (or (file-exists-p rgignore) (file-exists-p fdignore))
      (unless (y-or-n-p (format "Overwrite existing ignore files in %s? " dir))
        (user-error "Cancelled")))
    (let* ((raw (with-temp-buffer
                  (insert-file-contents template-file)
                  (buffer-string)))
           (expanded (+tim/rgignore--expand raw dir)))
      (with-temp-file rgignore (insert expanded))
      (with-temp-file fdignore (insert expanded)))
    (message "Created .rgignore and .fdignore from '%s' template in %s"
             name (abbreviate-file-name dir))))

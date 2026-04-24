;;; modules/files.el --- Slugify and template-based file creation -*- lexical-binding: t; -*-

;;; Slugify

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

;;; New File (template-based file creation)

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

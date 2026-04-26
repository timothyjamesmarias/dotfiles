;;; modules/rails-nav.el --- Rails navigation via +lookup dispatch -*- lexical-binding: t; -*-

;;; Partial path resolution (gd + gf)

(defun +tim/rails-partial-file-handler (_identifier)
  "Resolve a partial path to its underscore-prefixed file.
E.g. 'shared/sidebar' → app/views/shared/_sidebar.slim.
Handles both render calls and bare path strings."
  (let* ((path (thing-at-point 'filename t))
         (root (doom-project-root)))
    (when (and path root (not (string-empty-p path)))
      (let* ((parts (split-string path "/"))
             (basename (car (last parts)))
             (dir-parts (butlast parts))
             (view-dir (expand-file-name
                        (concat "app/views/"
                                (when dir-parts (concat (string-join dir-parts "/") "/")))
                        root)))
        (when (file-directory-p view-dir)
          (cl-loop for ext in '("slim" "haml" "erb"
                                "html.slim" "html.haml" "html.erb"
                                "text.erb" "json.jbuilder" "turbo_stream.erb")
                   for candidate = (expand-file-name
                                    (format "_%s.%s" basename ext)
                                    view-dir)
                   when (file-exists-p candidate)
                   return (progn (find-file candidate) t)))))))

;;; SCSS-from-template handler

(defun +tim/rails--css-class-at-point ()
  "Extract CSS class name at point in a view template, or nil."
  (save-excursion
    (let ((line (thing-at-point 'line t)))
      (when line
        (cond
         ;; Slim: .class-name or div.class-name
         ((and (derived-mode-p 'slim-mode)
               (string-match "\\.\\([a-zA-Z_-][a-zA-Z0-9_-]*\\)" line))
          (let ((classes (save-match-data
                           (let (result (start 0))
                             (while (string-match "\\.\\([a-zA-Z_-][a-zA-Z0-9_-]*\\)" line start)
                               (push (match-string 1 line) result)
                               (setq start (match-end 0)))
                             (nreverse result)))))
            (if (= (length classes) 1)
                (car classes)
              (completing-read "Class: " classes nil t))))
         ;; ERB/HTML: class="foo bar baz"
         ((string-match "class=\"\\([^\"]+\\)\"" line)
          (let ((classes (split-string (match-string 1 line) " " t)))
            (if (= (length classes) 1)
                (car classes)
              (completing-read "Class: " classes nil t))))
         ;; Ruby hash-style: class: "foo bar" or class: 'foo bar'
         ((string-match "class:\\s-*[\"']\\([^\"']+\\)[\"']" line)
          (let ((classes (split-string (match-string 1 line) " " t)))
            (if (= (length classes) 1)
                (car classes)
              (completing-read "Class: " classes nil t))))
         ;; Fallback: symbol at point if it looks like a class name
         (t
          (let ((sym (thing-at-point 'symbol t)))
            (when (and sym (string-match-p "\\`[a-zA-Z_-][a-zA-Z0-9_-]*\\'" sym))
              sym))))))))

(defun +tim/rails--stylesheet-dirs (root)
  "Return list of existing stylesheet directories in project ROOT."
  (cl-remove-if-not
   #'file-directory-p
   (mapcar (lambda (d) (expand-file-name d root))
           '("app/assets/stylesheets"
             "app/frontend"
             "app/javascript"))))

(defun +tim/rails--split-bem-class (class)
  "Split CLASS into (BLOCK . SUFFIX) at the last BEM separator.
Returns nil if CLASS has no BEM separator.
E.g. \"foo__bar--baz\" → (\"foo__bar\" . \"--baz\")
     \"foo__bar\" → (\"foo\" . \"__bar\")"
  (cond
   ((string-match "\\(.+\\)\\(--[a-zA-Z0-9_-]+\\)\\'" class)
    (cons (match-string 1 class) (match-string 2 class)))
   ((string-match "\\(.+\\)\\(__[a-zA-Z0-9_-]+\\)\\'" class)
    (cons (match-string 1 class) (match-string 2 class)))))

(defun +tim/rails--find-nested-selector (file start-line suffix)
  "In FILE starting at START-LINE, find &SUFFIX within the enclosing block.
Tracks brace depth to stay within scope. Returns line number or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (forward-line (1- start-line))
    (let ((depth 0) (found nil))
      (while (and (not (eobp)) (not found))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (setq depth (+ depth
                         (- (cl-count ?{ line)
                            (cl-count ?} line))))
          (when (string-match-p
                 (format "&%s\\b" (regexp-quote suffix))
                 line)
            (setq found (line-number-at-pos)))
          (when (<= depth 0)
            (goto-char (point-max))))
        (forward-line 1))
      found)))

(defun +tim/rails--rg-class (class dirs)
  "Grep for .CLASS in stylesheet DIRS. Returns list of (FILE . LINE) pairs."
  (let ((hits nil))
    (dolist (dir dirs)
      (with-temp-buffer
        (when (zerop (call-process "rg" nil t nil
                                   "--line-number" "--no-heading" "--with-filename"
                                   (format "\\.%s\\b" (regexp-quote class))
                                   dir))
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (when (string-match "\\`\\(.+\\):\\([0-9]+\\):" line)
                (push (cons (match-string 1 line)
                            (string-to-number (match-string 2 line)))
                      hits)))
            (forward-line 1)))))
    (nreverse hits)))

(defun +tim/rails-scss-lookup-handler (_identifier)
  "Lookup handler: jump from CSS class in a template to its SCSS/CSS definition.
Handles BEM nested selectors (e.g. &__content inside .block)."
  (when (derived-mode-p 'web-mode 'slim-mode 'haml-mode 'html-mode 'mhtml-mode)
    (when-let* ((class (+tim/rails--css-class-at-point))
                (root (doom-project-root))
                (dirs (+tim/rails--stylesheet-dirs root)))
      (let ((results nil))
        ;; Pass 1: direct grep for .classname
        (dolist (hit (+tim/rails--rg-class class dirs))
          (push (cons (format "%s:%d: .%s"
                              (file-relative-name (car hit) root)
                              (cdr hit) class)
                      hit)
                results))
        ;; Pass 2: BEM nested resolution if direct grep found nothing
        (unless results
          (let ((split (+tim/rails--split-bem-class class)))
            (while (and (null results) split)
              (let ((block (car split))
                    (suffix (cdr split)))
                (dolist (hit (+tim/rails--rg-class block dirs))
                  (when-let ((nested-line
                              (+tim/rails--find-nested-selector
                               (car hit) (cdr hit) suffix)))
                    (push (cons (format "%s:%d: &%s"
                                        (file-relative-name (car hit) root)
                                        nested-line suffix)
                                (cons (car hit) nested-line))
                          results)))
                ;; If still no results and block itself has a BEM separator,
                ;; try one level up (e.g. foo__bar--baz → find foo__bar first)
                (setq split (and (null results)
                                 (+tim/rails--split-bem-class block)))))))
        ;; Jump to result
        (when results
          (setq results (nreverse results))
          (let* ((pick (if (= (length results) 1)
                           (cdar results)
                         (let ((chosen (completing-read "SCSS match: "
                                                        (mapcar #'car results) nil t)))
                           (cdr (assoc chosen results)))))
                 (file (car pick))
                 (line (cdr pick)))
            (find-file file)
            (goto-char (point-min))
            (forward-line (1- line))
            t))))))

;;; Route jumping handler

(defun +tim/rails--current-action ()
  "Return the Ruby method name at or above point, or nil."
  (save-excursion
    (when (re-search-backward "^[[:space:]]*def[[:space:]]+\\([a-zA-Z_][a-zA-Z0-9_!?]*\\)" nil t)
      (match-string-no-properties 1))))

(defun +tim/rails--controller-info ()
  "If in a Rails controller file, return (CONTROLLER-PATH . ACTION) or nil."
  (when-let ((file (buffer-file-name)))
    (when (string-match "app/controllers/\\(.+\\)_controller\\.rb\\'" file)
      (let ((controller (match-string 1 file))
            (action (+tim/rails--current-action)))
        (cons controller action)))))

(defun +tim/rails--route-ref-at-point ()
  "In routes.rb, extract controller#action reference at point, or nil."
  (let ((line (thing-at-point 'line t)))
    (when line
      (cond
       ((string-match "\\(?:to:\\s-*\\)?['\"]\\([a-z_/]+\\)#\\([a-z_!?]+\\)['\"]" line)
        (cons (match-string 1 line) (match-string 2 line)))
       ((string-match "resources?\\s-+:\\([a-z_]+\\)" line)
        (cons (match-string 1 line) nil))
       ((string-match "controller:\\s-*[:'\"]+\\([a-z_/]+\\)" line)
        (cons (match-string 1 line) nil))))))

(defun +tim/rails-route-lookup-handler (_identifier)
  "Lookup handler: jump between controller actions and route definitions."
  (let ((root (doom-project-root)))
    (cond
     ;; In a controller → jump to routes.rb and search
     ((when-let ((info (+tim/rails--controller-info)))
        (let* ((controller (car info))
               (action (cdr info))
               (routes-file (expand-file-name "config/routes.rb" root))
               (pattern (if action
                            (format "%s\\|%s#%s"
                                    (file-name-nondirectory controller)
                                    controller action)
                          (file-name-nondirectory controller))))
          (when (file-exists-p routes-file)
            (find-file routes-file)
            (goto-char (point-min))
            (if (re-search-forward pattern nil t)
                (progn (beginning-of-line) t)
              t)))))
     ;; In routes.rb → jump to controller
     ((when-let* ((ref (+tim/rails--route-ref-at-point))
                  (controller (car ref))
                  (action (cdr ref))
                  (file (expand-file-name
                         (format "app/controllers/%s_controller.rb" controller)
                         root)))
        (when (file-exists-p file)
          (find-file file)
          (goto-char (point-min))
          (when action
            (re-search-forward
             (format "^[[:space:]]*def[[:space:]]+%s\\b" (regexp-quote action))
             nil t)
            (beginning-of-line))
          t))))))

;;; Combined dispatch + registration

(defun +tim/rails-nav-definition-handler (identifier)
  "Combined Rails lookup handler: tries partial, SCSS, and route handlers."
  (or (+tim/rails-partial-file-handler identifier)
      (+tim/rails-scss-lookup-handler identifier)
      (+tim/rails-route-lookup-handler identifier)))

;; :definition works via set-lookup-handlers!
(set-lookup-handlers! '+tim/rails-nav-mode
  :definition #'+tim/rails-nav-definition-handler)

;; :file must be registered manually (Doom bug: make-list 5 truncates cl-mapc,
;; skipping :file and :xref-backend handlers entirely).
(defun +tim/rails-nav-mode-setup-h ()
  "Register :file handler for gf."
  (if +tim/rails-nav-mode
      (add-hook '+lookup-file-functions #'+tim/rails-partial-file-handler nil 'local)
    (remove-hook '+lookup-file-functions #'+tim/rails-partial-file-handler 'local)))

(add-hook '+tim/rails-nav-mode-hook #'+tim/rails-nav-mode-setup-h)

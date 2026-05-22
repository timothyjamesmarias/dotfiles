;;; modules/rails-nav.el --- Rails navigation via dwim-nav rules -*- lexical-binding: t; -*-
;;
;; Registers context-aware navigation rules for Rails projects:
;;   - Partial file resolution (render "shared/sidebar" → _sidebar.slim)
;;   - SCSS class lookup from templates (with BEM nested selector support)
;;   - Controller ↔ routes.rb bidirectional jumping
;;
;; CSS class extraction is provided by `+dwim-nav-css-class-at-point'
;; in dwim-nav.el (shared across frameworks).  Rails-specific resolver
;; functions live here.

;;; Partial path resolution

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
                   return candidate))))))

(defun +tim/rails-partial-dwim-handler (_identifier)
  "DWIM handler: resolve partial path and jump."
  (when-let ((file (+tim/rails-partial-file-handler nil)))
    (find-file file)
    (+dwim-nav-result-jumped)))

;;; SCSS resolver (uses shared matcher from dwim-nav.el)

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

(defun +tim/rails--resolve-css-class (class)
  "Find SCSS/CSS definition for CLASS in Rails stylesheet dirs.
Returns list of plists (:label :file :line) or nil.
Handles BEM nested selectors (e.g. &__content inside .block)."
  (when-let* ((root (doom-project-root))
              (dirs (+tim/rails--stylesheet-dirs root)))
    (let ((results nil))
      ;; Pass 1: direct grep for .classname
      (dolist (hit (+tim/rails--rg-class class dirs))
        (push (list :label (format "%s:%d: .%s"
                                   (file-relative-name (car hit) root)
                                   (cdr hit) class)
                    :file (car hit)
                    :line (cdr hit))
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
                  (push (list :label (format "%s:%d: &%s"
                                             (file-relative-name (car hit) root)
                                             nested-line suffix)
                              :file (car hit)
                              :line nested-line)
                        results)))
              (setq split (and (null results)
                               (+tim/rails--split-bem-class block)))))))
      (nreverse results))))

(defun +tim/rails-scss-dwim-handler (_identifier)
  "DWIM handler: jump from CSS class in template to SCSS definition."
  (when-let* ((class (+dwim-nav-css-class-at-point))
              (hits (+tim/rails--resolve-css-class class)))
    (if (= (length hits) 1)
        (progn
          (+dwim-nav--jump-to (car hits))
          (+dwim-nav-result-jumped))
      (+dwim-nav-result-candidates hits))))

;;; Route jumping

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

(defun +tim/rails-routes-dwim-handler (_identifier)
  "DWIM handler: jump between controller actions and route definitions."
  (let ((root (doom-project-root)))
    (cond
     ;; In a controller → jump to routes.rb
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
            (when (re-search-forward pattern nil t)
              (beginning-of-line))
            (+dwim-nav-result-jumped)))))
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
          (+dwim-nav-result-jumped)))))))

;;; Rule registration

(+dwim-nav-rule! rails-partial
  :modes (web-mode slim-mode haml-mode html-mode mhtml-mode)
  :frameworks (rails)
  :predicate (lambda ()
               (let ((path (thing-at-point 'filename t)))
                 (and path (not (string-empty-p path)))))
  :handler #'+tim/rails-partial-dwim-handler
  :priority 10
  :label "Rails partial")

(+dwim-nav-rule! rails-scss
  :modes (web-mode slim-mode haml-mode html-mode mhtml-mode)
  :frameworks (rails)
  :predicate #'+dwim-nav-css-class-at-point
  :handler #'+tim/rails-scss-dwim-handler
  :priority 20
  :label "Rails SCSS")

(+dwim-nav-rule! rails-routes
  :modes (ruby-mode ruby-ts-mode)
  :frameworks (rails)
  :predicate (lambda ()
               (or (+tim/rails--controller-info)
                   (and (buffer-file-name)
                        (string-match-p "routes\\.rb\\'" (buffer-file-name)))))
  :handler #'+tim/rails-routes-dwim-handler
  :priority 10
  :label "Rails routes")

;;; gf handler (stays on +lookup-file-functions, separate from dwim-nav)

(defun +tim/rails-nav-mode-setup-h ()
  "Register :file handler for gf."
  (if +tim/rails-nav-mode
      (add-hook '+lookup-file-functions #'+tim/rails-partial-file-handler nil 'local)
    (remove-hook '+lookup-file-functions #'+tim/rails-partial-file-handler 'local)))

(add-hook '+tim/rails-nav-mode-hook #'+tim/rails-nav-mode-setup-h)

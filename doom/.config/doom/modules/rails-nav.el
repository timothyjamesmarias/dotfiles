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
  "Resolve a render path to its view file.
Tries both partial (underscore-prefixed) and full template paths.
E.g. 'shared/sidebar' → app/views/shared/_sidebar.slim
     'blog/by_category' → app/views/blog/by_category.html.slim
Uses tree-sitter to extract the full string content (handles paths
with /) when available, falls back to `thing-at-point'."
  (let* ((path (or (+dwim-nav-treesit-string-at-point)
                   (thing-at-point 'filename t)))
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
          (catch 'found
            (dolist (prefix '("_" ""))
              (dolist (ext '("slim" "haml" "erb"
                             "html.slim" "html.haml" "html.erb"
                             "text.erb" "json.jbuilder" "turbo_stream.erb"))
                (let ((candidate (expand-file-name
                                  (format "%s%s.%s" prefix basename ext)
                                  view-dir)))
                  (when (file-exists-p candidate)
                    (throw 'found candidate)))))))))))

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

(defun +tim/rails--on-action-def-p ()
  "Return non-nil if point is on or inside a controller action definition.
Specifically: cursor is on a `def method_name` line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[[:space:]]*def[[:space:]]+[a-zA-Z_]")))

(defun +tim/rails-routes-dwim-handler (_identifier)
  "DWIM handler: jump between controller actions and route definitions."
  (let ((root (doom-project-root)))
    (cond
     ;; On a `def action` line in a controller → jump to routes.rb
     ((when-let ((info (+tim/rails--controller-info)))
        (let* ((controller (car info))
               (action (cdr info))
               (routes-file (expand-file-name "config/routes.rb" root))
               (pattern (if action
                            (format "%s#%s\\|%s.*%s"
                                    controller action
                                    (file-name-nondirectory controller)
                                    action)
                          (file-name-nondirectory controller))))
          (when (file-exists-p routes-file)
            ;; Search without opening the file first — only jump if found
            (let ((found nil))
              (with-temp-buffer
                (insert-file-contents routes-file)
                (goto-char (point-min))
                (when (re-search-forward pattern nil t)
                  (setq found (line-number-at-pos))))
              (when found
                (find-file routes-file)
                (goto-char (point-min))
                (forward-line (1- found))
                (+dwim-nav-result-jumped)))))))
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

;;; Render call resolution

(defun +tim/rails--render-target ()
  "Extract the render target from the current line.
Returns (TYPE . VALUE) where TYPE is one of:
  `template'  — explicit template path (string)
  `action'    — action name (symbol or keyword arg)
  `partial'   — explicit partial path (string)
Or nil if not on a render call."
  (let ((line (thing-at-point 'line t)))
    (when (and line (string-match-p "\\brender\\b" line))
      (cond
       ;; render template: "users/show"
       ((string-match "render\\s-+template:\\s-*['\"]\\([^'\"]+\\)['\"]" line)
        (cons 'template (match-string 1 line)))
       ;; render partial: "shared/sidebar" — handled by existing partial rule
       ((string-match "render\\s-+partial:\\s-*['\"]\\([^'\"]+\\)['\"]" line)
        (cons 'partial (match-string 1 line)))
       ;; render action: :edit
       ((string-match "render\\s-+action:\\s-*:\\([a-zA-Z0-9_]+\\)" line)
        (cons 'action (match-string 1 line)))
       ;; render :edit (bare symbol after render)
       ((string-match "render\\s-+:\\([a-zA-Z0-9_]+\\)" line)
        (cons 'action (match-string 1 line)))
       ;; render "path/to/template" — already handled by partial rule via tree-sitter
       ))))

(defun +tim/rails--find-view-template (controller-path action root)
  "Find the view template for CONTROLLER-PATH and ACTION under ROOT.
Returns file path or nil."
  (let ((view-dir (expand-file-name
                   (format "app/views/%s/" controller-path) root)))
    (when (file-directory-p view-dir)
      (catch 'found
        (dolist (ext '("html.slim" "html.haml" "html.erb"
                       "slim" "haml" "erb"
                       "json.jbuilder" "turbo_stream.erb"))
          (let ((candidate (expand-file-name
                            (format "%s.%s" action ext) view-dir)))
            (when (file-exists-p candidate)
              (throw 'found candidate))))))))

(defun +tim/rails-render-dwim-handler (_identifier)
  "Jump from a render call to the target view template."
  (when-let ((target (+tim/rails--render-target))
             (root (doom-project-root)))
    (let ((type (car target))
          (value (cdr target)))
      (pcase type
        ('action
         ;; render :edit or render action: :new
         ;; Resolve relative to current controller
         (when-let* ((info (+tim/rails--controller-info))
                     (controller (car info))
                     (file (+tim/rails--find-view-template controller value root)))
           (find-file file)
           (+dwim-nav-result-jumped)))
        ('template
         ;; render template: "users/show"
         ;; Resolve as a full path under app/views/
         (let* ((parts (split-string value "/"))
                (action (car (last parts)))
                (dir-path (string-join (butlast parts) "/")))
           (when-let ((file (+tim/rails--find-view-template dir-path action root)))
             (find-file file)
             (+dwim-nav-result-jumped))))))))

(+dwim-nav-rule! rails-render
  :modes (ruby-mode ruby-ts-mode)
  :frameworks (rails)
  :predicate (lambda ()
               (when-let ((target (+tim/rails--render-target)))
                 (memq (car target) '(action template))))
  :handler #'+tim/rails-render-dwim-handler
  :priority 10
  :label "Rails render")

;;; Rule registration

(+dwim-nav-rule! rails-partial
  :modes (web-mode slim-mode haml-mode html-mode mhtml-mode ruby-mode ruby-ts-mode)
  :frameworks (rails)
  :predicate (lambda ()
               (let ((path (or (+dwim-nav-treesit-string-at-point)
                               (thing-at-point 'filename t))))
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
               (or (and (+tim/rails--controller-info)
                        (+tim/rails--on-action-def-p))
                   (and (buffer-file-name)
                        (string-match-p "routes\\.rb\\'" (buffer-file-name)))))
  :handler #'+tim/rails-routes-dwim-handler
  :priority 10
  :label "Rails routes")

;;; View → Controller jumping

(defun +tim/rails--view-to-controller ()
  "From a view file, return (CONTROLLER-FILE . ACTION) or nil."
  (when-let* ((file (buffer-file-name))
              (root (doom-project-root)))
    (when (string-match "app/views/\\(.+\\)/\\([^_][^/]*\\)\\." file)
      (let* ((controller-path (match-string 1 file))
             (action (match-string 2 file))
             (controller-file (expand-file-name
                               (format "app/controllers/%s_controller.rb"
                                       controller-path)
                               root)))
        (when (file-exists-p controller-file)
          (cons controller-file action))))))

(defun +tim/rails-view-controller-dwim-handler (_identifier)
  "DWIM handler: from a view file, jump to the controller action."
  (when-let ((info (+tim/rails--view-to-controller)))
    (find-file (car info))
    (goto-char (point-min))
    (when (cdr info)
      (when (re-search-forward
             (format "^[[:space:]]*def[[:space:]]+%s\\b" (regexp-quote (cdr info)))
             nil t)
        (beginning-of-line)))
    (+dwim-nav-result-jumped)))

(+dwim-nav-rule! rails-view-controller
  :modes (web-mode slim-mode haml-mode html-mode mhtml-mode)
  :frameworks (rails)
  :predicate (lambda ()
               (when-let ((file (buffer-file-name)))
                 (and (string-match-p "app/views/" file)
                      ;; Not a partial (partials start with _)
                      (not (string-match-p "/_[^/]*\\'" file)))))
  :handler #'+tim/rails-view-controller-dwim-handler
  :priority 15
  :label "Rails view→controller")

;;; Constant → File jumping (via projectile-rails)

(defun +tim/rails-constant-dwim-handler (_identifier)
  "DWIM handler: jump to the file defining the Ruby constant at point.
Delegates to `projectile-rails-goto-constant-at-point'."
  (condition-case nil
      (progn
        (projectile-rails-goto-constant-at-point)
        (+dwim-nav-result-jumped))
    ((error user-error) nil)))

(+dwim-nav-rule! rails-constant
  :modes (ruby-mode ruby-ts-mode)
  :frameworks (rails)
  :predicate (lambda ()
               (when-let ((sym (thing-at-point 'symbol t)))
                 (string-match-p "\\`\\(::\\)?[A-Z][a-zA-Z0-9_:]*\\'" sym)))
  :handler #'+tim/rails-constant-dwim-handler
  :priority 40
  :label "Rails constant")

;;; Keyword-based navigation (associations, callbacks, layouts)

(defvar +tim/rails--association-keywords-re
  "^[[:space:]]*\\(belongs_to\\|has_one\\|has_many\\|has_and_belongs_to_many\\)\\b"
  "Regex matching Rails association declaration lines.")

(defvar +tim/rails--callback-keywords-re
  (concat "^[[:space:]]*\\("
          (mapconcat #'identity
                     '("before_validation" "after_validation"
                       "before_save" "around_save" "after_save"
                       "before_create" "around_create" "after_create"
                       "before_update" "around_update" "after_update"
                       "before_destroy" "around_destroy" "after_destroy"
                       "after_commit" "after_create_commit"
                       "after_update_commit" "after_destroy_commit"
                       "after_initialize" "after_find" "after_touch")
                     "\\|")
          "\\)\\b")
  "Regex matching Rails callback declaration lines.")

(defun +tim/rails--association-symbol-at-cursor ()
  "Determine which association symbol to resolve based on cursor position.
If cursor is on a `through:` or `source:` value, use that symbol.
Otherwise use the primary association symbol (first :symbol after the keyword).
Returns (SYMBOL . CONTEXT) where CONTEXT is `primary', `through', or `source'."
  (let* ((line (thing-at-point 'line t))
         (sym (thing-at-point 'symbol t))
         (col (- (point) (line-beginning-position))))
    (when (and sym line)
      ;; Check if cursor is on a through: or source: value
      (cond
       ((and (string-match "through:\\s-*:\\([a-zA-Z0-9_]+\\)" line)
             (let ((start (match-beginning 1))
                   (end (match-end 1)))
               (<= start col)
               (< col end)))
        (cons (match-string 1 line) 'through))
       ((and (string-match "source:\\s-*:\\([a-zA-Z0-9_]+\\)" line)
             (let ((start (match-beginning 1))
                   (end (match-end 1)))
               (<= start col)
               (< col end)))
        (cons (match-string 1 line) 'source))
       ;; Default: primary association symbol
       ((string-match
         "^[[:space:]]*\\(belongs_to\\|has_one\\|has_many\\|has_and_belongs_to_many\\)\\s-+:\\([a-zA-Z0-9_]+\\)"
         line)
        (cons (match-string 2 line) 'primary))))))

(defun +tim/rails-association-dwim-handler (_identifier)
  "Jump from association declaration to the associated model file.
Cursor-aware: if on a `through:` or `source:` value, resolves that
association instead of the primary one."
  (let ((line (thing-at-point 'line t)))
    (when (string-match
           "^[[:space:]]*\\(belongs_to\\|has_one\\|has_many\\|has_and_belongs_to_many\\)\\s-+:\\([a-zA-Z0-9_]+\\)"
           line)
      (let* ((keyword (match-string 1 line))
             (cursor-info (+tim/rails--association-symbol-at-cursor))
             (symbol (car cursor-info))
             (context (cdr cursor-info))
             (root (doom-project-root))
             (explicit-class
              (when (and (eq context 'primary)
                         (string-match "class_name:\\s-*['\"]\\([^'\"]+\\)['\"]" line))
                (match-string 1 line)))
             ;; through/source values are always singular association names
             ;; primary symbols need singularization for has_many
             (model-name
              (cond
               (explicit-class
                (downcase (replace-regexp-in-string "::" "/" explicit-class)))
               ((and (eq context 'primary)
                     (member keyword '("has_many" "has_and_belongs_to_many")))
                (inflection-singularize-string symbol))
               ;; through: value — singularize (e.g., :taggings → tagging)
               ((eq context 'through)
                (inflection-singularize-string symbol))
               ;; source: value and belongs_to/has_one — already singular
               (t symbol)))
             (file (expand-file-name (format "app/models/%s.rb" model-name) root)))
        (when (file-exists-p file)
          (find-file file)
          (+dwim-nav-result-jumped))))))

(defun +tim/rails-callback-dwim-handler (_identifier)
  "Jump from callback symbol to its method definition in current buffer."
  (when-let ((sym (thing-at-point 'symbol t)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (format "^[[:space:]]*def[[:space:]]+%s\\b" (regexp-quote sym))
             nil t)
        (beginning-of-line)
        (+dwim-nav-result-jumped)))))

(defun +tim/rails-layout-dwim-handler (_identifier)
  "Jump from `layout \"name\"` to the layout template file."
  (let ((line (thing-at-point 'line t)))
    (when (string-match "layout[[:space:]]+['\"]\\([^'\"]+\\)['\"]" line)
      (let* ((name (match-string 1 line))
             (root (doom-project-root))
             (dir (expand-file-name "app/views/layouts/" root)))
        (when (file-directory-p dir)
          (cl-loop for ext in '("html.slim" "html.haml" "html.erb"
                                "slim" "haml" "erb")
                   for candidate = (expand-file-name (format "%s.%s" name ext) dir)
                   when (file-exists-p candidate)
                   return (progn
                            (find-file candidate)
                            (+dwim-nav-result-jumped))))))))

(+dwim-nav-rule! rails-association
  :modes (ruby-mode ruby-ts-mode)
  :frameworks (rails)
  :predicate (lambda ()
               (save-excursion
                 (beginning-of-line)
                 (looking-at-p +tim/rails--association-keywords-re)))
  :handler #'+tim/rails-association-dwim-handler
  :priority 25
  :label "Rails association")

(+dwim-nav-rule! rails-callback
  :modes (ruby-mode ruby-ts-mode)
  :frameworks (rails)
  :predicate (lambda ()
               (save-excursion
                 (beginning-of-line)
                 (looking-at-p +tim/rails--callback-keywords-re)))
  :handler #'+tim/rails-callback-dwim-handler
  :priority 25
  :label "Rails callback")

(+dwim-nav-rule! rails-layout
  :modes (ruby-mode ruby-ts-mode)
  :frameworks (rails)
  :predicate (lambda ()
               (save-excursion
                 (beginning-of-line)
                 (looking-at-p "^[[:space:]]*layout[[:space:]]+['\"]")))
  :handler #'+tim/rails-layout-dwim-handler
  :priority 25
  :label "Rails layout")

;;; Instance variable resolution in views

(defun +tim/rails--ivar-at-point ()
  "Extract instance variable name at point, including the @.
Uses tree-sitter when available (returns \"@search_terms\" directly
from the `instance_variable' node).  Falls back to checking the
character before `thing-at-point' bounds for template modes.
Returns e.g. \"@search_terms\" or nil."
  (or
   ;; Tree-sitter path (ruby-ts-mode)
   (+dwim-nav-treesit-ivar-at-point)
   ;; Fallback for non-ts modes (web-mode, slim-mode, etc.)
   (let* ((bounds (bounds-of-thing-at-point 'symbol))
          (sym (and bounds (buffer-substring-no-properties (car bounds) (cdr bounds)))))
     (when sym
       (cond
        ;; @ is included in the symbol (some modes)
        ((string-prefix-p "@" sym) sym)
        ;; @@ class variable
        ((and (> (car bounds) (1+ (point-min)))
              (= (char-before (car bounds)) ?@)
              (= (char-before (1- (car bounds))) ?@))
         (concat "@@" sym))
        ;; @ is before the symbol boundary
        ((and (> (car bounds) (point-min))
              (= (char-before (car bounds)) ?@))
         (concat "@" sym)))))))

(defun +tim/rails-ivar-in-view-dwim-handler (_identifier)
  "Jump from @ivar in a view to its assignment in the controller."
  (when-let* ((ivar (+tim/rails--ivar-at-point))
              (info (+tim/rails--view-to-controller))
              (controller-file (car info))
              (action (cdr info)))
    (let ((assignment-re (format "%s\\s-*\\(||\\)?=[^=]" (regexp-quote ivar)))
          (action-re (format "^[[:space:]]*def[[:space:]]+%s\\b" (regexp-quote action)))
          (found-line nil))
      ;; Search within the controller action method first
      (with-temp-buffer
        (insert-file-contents controller-file)
        (goto-char (point-min))
        (when (re-search-forward action-re nil t)
          (while (and (not found-line)
                      (not (eobp))
                      (re-search-forward
                       (concat "\\(" assignment-re "\\)\\|\\(^[[:space:]]*def[[:space:]]\\)")
                       nil t))
            (if (match-string 2)
                (goto-char (point-max))  ; hit next def, stop
              (setq found-line (line-number-at-pos)))))
        ;; Fallback: search entire file (before_action, etc.)
        (unless found-line
          (goto-char (point-min))
          (when (re-search-forward assignment-re nil t)
            (setq found-line (line-number-at-pos)))))
      (when found-line
        (find-file controller-file)
        (goto-char (point-min))
        (forward-line (1- found-line))
        (+dwim-nav-result-jumped)))))

(+dwim-nav-rule! rails-ivar-in-view
  :modes (web-mode slim-mode haml-mode html-mode mhtml-mode)
  :frameworks (rails)
  :predicate (lambda ()
               (and (buffer-file-name)
                    (string-match-p "app/views/" (buffer-file-name))
                    (not (string-match-p "/_[^/]*\\'" (buffer-file-name)))
                    (+tim/rails--ivar-at-point)))
  :handler #'+tim/rails-ivar-in-view-dwim-handler
  :priority 30
  :label "Rails @ivar→controller")

;;; Instance variable resolution in Ruby files

(defun +tim/rails--controller-view-dir ()
  "If in a controller, return the corresponding views directory or nil.
E.g. app/controllers/blog_controller.rb → app/views/blog/."
  (when-let* ((file (buffer-file-name))
              (root (doom-project-root)))
    (when (string-match "app/controllers/\\(.+\\)_controller\\.rb\\'" file)
      (let ((dir (expand-file-name
                  (format "app/views/%s/" (match-string 1 file))
                  root)))
        (when (file-directory-p dir) dir)))))

(defun +tim/rails-ivar-in-ruby-dwim-handler (_identifier)
  "Jump from @ivar in a Ruby file: show assignment + view usages as candidates."
  (when-let ((ivar (+tim/rails--ivar-at-point)))
    (let ((candidates nil)
          (ivar-re (regexp-quote ivar))
          (root (doom-project-root)))
      ;; 1. Find assignments in current buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                (format "%s\\s-*\\(||\\)?=[^=]" ivar-re) nil t)
          (push (list :label (format "%s:%d (assignment)"
                                     (file-name-nondirectory (buffer-file-name))
                                     (line-number-at-pos))
                      :file (buffer-file-name)
                      :line (line-number-at-pos))
                candidates)))
      ;; 2. Find usages in view templates (search all views, not just controller-specific)
      (let ((views-dir (expand-file-name "app/views/" root)))
        (when (file-directory-p views-dir)
          (with-temp-buffer
            (when (zerop (call-process "rg" nil t nil
                                       "--line-number" "--no-heading" "--with-filename"
                                       ivar-re views-dir))
              (goto-char (point-min))
              (while (not (eobp))
                (let ((line (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))))
                  (when (string-match "\\`\\(.+\\):\\([0-9]+\\):" line)
                    (let ((file (match-string 1 line))
                          (lnum (string-to-number (match-string 2 line))))
                      (push (list :label (format "%s:%d (view)"
                                                 (file-relative-name file root)
                                                 lnum)
                                  :file file
                                  :line lnum)
                            candidates))))
                (forward-line 1))))))
      ;; Return results
      (when candidates
        (setq candidates (nreverse candidates))
        (if (= (length candidates) 1)
            (progn
              (+dwim-nav--jump-to (car candidates))
              (+dwim-nav-result-jumped))
          (+dwim-nav-result-candidates candidates))))))

(+dwim-nav-rule! rails-ivar-in-ruby
  :modes (ruby-mode ruby-ts-mode)
  :frameworks (rails)
  :predicate #'+tim/rails--ivar-at-point
  :handler #'+tim/rails-ivar-in-ruby-dwim-handler
  :priority 30
  :label "Rails @ivar")

;;; gf handler (stays on +lookup-file-functions, separate from dwim-nav)

(defun +tim/rails-nav-mode-setup-h ()
  "Register :file handler for gf."
  (if +tim/rails-nav-mode
      (add-hook '+lookup-file-functions #'+tim/rails-partial-file-handler nil 'local)
    (remove-hook '+lookup-file-functions #'+tim/rails-partial-file-handler 'local)))

(add-hook '+tim/rails-nav-mode-hook #'+tim/rails-nav-mode-setup-h)

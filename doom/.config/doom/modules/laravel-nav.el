;;; modules/laravel-nav.el --- Laravel navigation via dwim-nav rules -*- lexical-binding: t; -*-
;;
;; Context-aware navigation rules for Laravel projects:
;;   - Blade component resolution (<x-alert>, <x-forms.input>)
;;   - Controller ↔ route bidirectional jumping
;;
;; Complements intelephense (eglot): LSP handles PHP class/method
;; resolution.  These rules handle Blade templates and route strings
;; where LSP has no information.

;;; Blade component resolution

(defun +tim/laravel-blade-component-at-point ()
  "Extract Blade component name from an <x-...> tag at point, or nil."
  (let ((line (thing-at-point 'line t)))
    (when (and line (string-match "</?x-\\([[:alnum:]._-]+\\)" line))
      (match-string 1 line))))

(defun +tim/laravel--component-candidates (name root)
  "Find candidate files for Blade component NAME in project ROOT.
Returns list of plists (:label :file :line) or nil."
  (let* ((path (replace-regexp-in-string "\\." "/" name))
         (pascal-path (mapconcat
                       (lambda (seg)
                         (mapconcat #'capitalize (split-string seg "-") ""))
                       (split-string path "/") "/"))
         (checks
          (list
           ;; Anonymous Blade component
           (cons (format "resources/views/components/%s.blade.php" path)
                 "Blade template")
           ;; Directory component with index
           (cons (format "resources/views/components/%s/index.blade.php" path)
                 "Blade template (index)")
           ;; Class-based component
           (cons (format "app/View/Components/%s.php" pascal-path)
                 "Component class")))
         (candidates nil))
    (dolist (check checks)
      (let ((file (expand-file-name (car check) root)))
        (when (file-exists-p file)
          (push (list :label (format "%s — %s"
                                     (file-relative-name file root)
                                     (cdr check))
                      :file file
                      :line 1)
                candidates))))
    (nreverse candidates)))

(defun +tim/laravel-blade-component-handler (_identifier)
  "DWIM handler: jump from <x-component> tag to the component file."
  (when-let* ((name (+tim/laravel-blade-component-at-point))
              (root (doom-project-root))
              (hits (+tim/laravel--component-candidates name root)))
    (if (= (length hits) 1)
        (progn
          (+dwim-nav--jump-to (car hits))
          (+dwim-nav-result-jumped))
      (+dwim-nav-result-candidates hits))))

(+dwim-nav-rule! laravel-blade-component
  :modes (web-mode php-mode php-ts-mode html-mode)
  :frameworks (laravel)
  :predicate #'+tim/laravel-blade-component-at-point
  :handler #'+tim/laravel-blade-component-handler
  :priority 10
  :label "Laravel component")

;;; Controller ↔ Route jumping

(defun +tim/laravel--controller-info ()
  "If in a Laravel controller file, return (CLASS-NAME . METHOD) or nil.
CLASS-NAME is the short name (e.g. \"UserController\")."
  (when-let ((file (buffer-file-name)))
    (when (string-match "app/Http/Controllers/\\(.+\\)\\.php\\'" file)
      (let* ((rel (match-string 1 file))
             (class-name (file-name-nondirectory rel))
             (method (save-excursion
                       (when (re-search-backward
                              "function[[:space:]]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" nil t)
                         (match-string-no-properties 1)))))
        (cons class-name method)))))

(defun +tim/laravel--route-ref-at-point ()
  "In a route file, extract controller + method reference at point, or nil.
Returns (CONTROLLER . METHOD) or nil."
  (let ((line (thing-at-point 'line t)))
    (when line
      (cond
       ;; [UserController::class, 'index']
       ((string-match "\\[\\\\?\\([A-Za-z_\\\\]+Controller\\)::class[[:space:]]*,[[:space:]]*['\"]\\([a-zA-Z_]+\\)['\"]" line)
        (cons (car (last (split-string (match-string 1 line) "\\\\")))
              (match-string 2 line)))
       ;; 'UserController@index'
       ((string-match "['\"]\\([A-Za-z_\\\\]+Controller\\)@\\([a-zA-Z_]+\\)['\"]" line)
        (cons (car (last (split-string (match-string 1 line) "\\\\")))
              (match-string 2 line)))
       ;; Route::resource('users', UserController::class)
       ((string-match "\\([A-Za-z_\\\\]+Controller\\)::class" line)
        (cons (car (last (split-string (match-string 1 line) "\\\\")))
              nil))))))

(defun +tim/laravel--find-controller-file (class-name root)
  "Find the file for a Laravel controller CLASS-NAME under ROOT.
Searches app/Http/Controllers/ recursively. Returns path or nil."
  (let ((target (format "%s.php" class-name)))
    (car (directory-files-recursively
          (expand-file-name "app/Http/Controllers" root)
          (format "\\`%s\\'" (regexp-quote target))))))

(defun +tim/laravel--route-files (root)
  "Return list of existing route files in project ROOT."
  (cl-remove-if-not
   #'file-exists-p
   (mapcar (lambda (f) (expand-file-name f root))
           '("routes/web.php" "routes/api.php"
             "routes/channels.php" "routes/console.php"))))

(defun +tim/laravel-routes-dwim-handler (_identifier)
  "DWIM handler: jump between controller methods and route definitions."
  (let ((root (doom-project-root)))
    (cond
     ;; In a controller → find matching route
     ((when-let ((info (+tim/laravel--controller-info)))
        (let* ((class-name (car info))
               (method (cdr info))
               (route-files (+tim/laravel--route-files root))
               (pattern (if method
                            (format "%s.*%s\\|%s@%s"
                                    (regexp-quote class-name)
                                    (regexp-quote method)
                                    (regexp-quote class-name)
                                    (regexp-quote method))
                          (regexp-quote class-name)))
               (hits nil))
          (dolist (rf route-files)
            (with-temp-buffer
              (insert-file-contents rf)
              (goto-char (point-min))
              (while (re-search-forward pattern nil t)
                (push (list :label (format "%s:%d"
                                           (file-relative-name rf root)
                                           (line-number-at-pos))
                            :file rf
                            :line (line-number-at-pos))
                      hits))))
          (when hits
            (setq hits (nreverse hits))
            (if (= (length hits) 1)
                (progn
                  (+dwim-nav--jump-to (car hits))
                  (+dwim-nav-result-jumped))
              (+dwim-nav-result-candidates hits))))))
     ;; In a route file → jump to controller
     ((when-let* ((ref (+tim/laravel--route-ref-at-point))
                  (class-name (car ref))
                  (method (cdr ref))
                  (file (+tim/laravel--find-controller-file class-name root)))
        (find-file file)
        (goto-char (point-min))
        (when method
          (when (re-search-forward
                 (format "function[[:space:]]+%s\\b" (regexp-quote method))
                 nil t)
            (beginning-of-line)))
        (+dwim-nav-result-jumped))))))

(+dwim-nav-rule! laravel-routes
  :modes (php-mode php-ts-mode)
  :frameworks (laravel)
  :predicate (lambda ()
               (or (+tim/laravel--controller-info)
                   (when-let ((file (buffer-file-name)))
                     (string-match-p "routes/\\(web\\|api\\|channels\\|console\\)\\.php\\'" file))))
  :handler #'+tim/laravel-routes-dwim-handler
  :priority 10
  :label "Laravel routes")

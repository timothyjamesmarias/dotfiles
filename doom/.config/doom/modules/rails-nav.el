;;; modules/rails-nav.el --- Rails navigation via +lookup dispatch -*- lexical-binding: t; -*-

;;; Bridge handler — wires projectile-rails into Doom's lookup chain

(defun +tim/rails-lookup-definition-handler (&optional _identifier)
  "Lookup handler that delegates to projectile-rails-goto-file-at-point.
Returns non-nil if navigation succeeded, nil to fall through."
  (when (bound-and-true-p projectile-rails-mode)
    (let ((buf (current-buffer)))
      (ignore-errors
        (projectile-rails-goto-file-at-point)
        (not (eq buf (current-buffer)))))))

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

(defun +tim/rails-scss-lookup-handler (&optional _identifier)
  "Lookup handler: jump from CSS class in a template to its SCSS/CSS definition."
  (when (and (bound-and-true-p +tim/rails-nav-mode)
             (derived-mode-p 'web-mode 'slim-mode 'haml-mode 'html-mode 'mhtml-mode))
    (when-let* ((class (+tim/rails--css-class-at-point))
                (root (doom-project-root))
                (dirs (+tim/rails--stylesheet-dirs root)))
      (let ((results nil))
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
                    (push (cons (format "%s:%s: %s"
                                        (file-relative-name (match-string 1 line) root)
                                        (match-string 2 line)
                                        (substring line (match-end 0)))
                                (cons (match-string 1 line)
                                      (string-to-number (match-string 2 line))))
                          results)))
                (forward-line 1)))))
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
  "If in a Rails controller file, return (CONTROLLER-PATH . ACTION) or nil.
CONTROLLER-PATH is e.g. \"users\" or \"admin/users\"."
  (when-let ((file (buffer-file-name)))
    (when (string-match "app/controllers/\\(.+\\)_controller\\.rb\\'" file)
      (let ((controller (match-string 1 file))
            (action (+tim/rails--current-action)))
        (cons controller action)))))

(defun +tim/rails--route-ref-at-point ()
  "In routes.rb, extract controller#action reference at point, or nil.
Returns (CONTROLLER . ACTION) or (CONTROLLER . nil)."
  (let ((line (thing-at-point 'line t)))
    (when line
      (cond
       ;; to: 'controller#action' or 'controller#action'
       ((string-match "\\(?:to:\\s-*\\)?['\"]\\([a-z_/]+\\)#\\([a-z_!?]+\\)['\"]" line)
        (cons (match-string 1 line) (match-string 2 line)))
       ;; resources :name or resource :name
       ((string-match "resources?\\s-+:\\([a-z_]+\\)" line)
        (cons (match-string 1 line) nil))
       ;; controller: :name or controller: 'name'
       ((string-match "controller:\\s-*[:'\"]+\\([a-z_/]+\\)" line)
        (cons (match-string 1 line) nil))))))

(defun +tim/rails-route-lookup-handler (&optional _identifier)
  "Lookup handler: jump between controller actions and route definitions."
  (when (bound-and-true-p +tim/rails-nav-mode)
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
                ;; Opened routes but couldn't find match — still navigated
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
            t)))))))

;;; Combined dispatch handler

(defun +tim/rails-nav-definition-handler (identifier)
  "Combined Rails lookup handler: tries bridge, SCSS, and route handlers."
  (or (+tim/rails-lookup-definition-handler identifier)
      (+tim/rails-scss-lookup-handler identifier)
      (+tim/rails-route-lookup-handler identifier)))

;;; Register on the minor mode

(after! lookup
  (set-lookup-handlers! '+tim/rails-nav-mode
    :definition #'+tim/rails-nav-definition-handler))

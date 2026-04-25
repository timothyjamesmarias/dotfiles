;;; modules/framework-detect.el --- Per-project framework detection -*- lexical-binding: t; -*-

(defvar +tim/framework-cache (make-hash-table :test 'equal)
  "Cache of project-root -> detected framework symbol.")

(defvar-local +tim/framework-override nil
  "Override framework detection. Set via .dir-locals.el.
Accepted values: rails, laravel, spring-boot.")

(define-minor-mode +tim/rails-nav-mode
  "Minor mode enabling Rails-specific lookup handlers."
  :lighter " Rails"
  :group 'tim)

(defun +tim/detect-framework (root)
  "Detect framework type for project at ROOT. Returns a symbol or nil."
  (or +tim/framework-override
      (gethash root +tim/framework-cache)
      (puthash root
               (cond
                ((and (file-exists-p (expand-file-name "Gemfile" root))
                      (file-exists-p (expand-file-name "config/routes.rb" root)))
                 'rails)
                ((and (file-exists-p (expand-file-name "composer.json" root))
                      (file-exists-p (expand-file-name "artisan" root)))
                 'laravel)
                ((or (file-exists-p (expand-file-name "build.gradle" root))
                     (file-exists-p (expand-file-name "build.gradle.kts" root)))
                 'spring-boot)
                (t nil))
               +tim/framework-cache)))

(defun +tim/framework-activate-h ()
  "Activate framework-specific nav mode for the current buffer."
  (when-let ((root (doom-project-root)))
    (pcase (+tim/detect-framework root)
      ('rails (+tim/rails-nav-mode +1)))))

(add-hook 'find-file-hook #'+tim/framework-activate-h)

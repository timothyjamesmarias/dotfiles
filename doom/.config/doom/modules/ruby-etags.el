;;; modules/ruby-etags.el --- Etags for Ruby/Rails projects -*- lexical-binding: t; -*-

(defvar +tim/ruby-ctags-command
  "ctags -e -R --languages=Ruby --exclude=.git --exclude=node_modules --exclude=tmp --exclude=log --exclude=vendor/bundle --exclude=public"
  "Command to generate an Emacs TAGS file for Ruby projects.")

(defvar +tim/ruby-ctags-dirs
  '("app" "lib" "config" "db" "test" "spec")
  "Directories to index when generating tags for a Rails project.
Falls back to project root if none of these exist.")

(defvar +tim/ruby-ctags-timer nil
  "Debounce timer for auto-regenerating tags on save.")

(defun +tim/ruby-ctags-generate ()
  "Generate TAGS file for the current Ruby/Rails project."
  (interactive)
  (when-let ((root (doom-project-root)))
    (let* ((default-directory root)
           (dirs (cl-remove-if-not
                  (lambda (d) (file-directory-p (expand-file-name d root)))
                  +tim/ruby-ctags-dirs))
           (target (if dirs (string-join dirs " ") "."))
           (cmd (format "%s %s" +tim/ruby-ctags-command target)))
      (message "Generating TAGS in %s..." root)
      (set-process-sentinel
       (start-process-shell-command "ruby-ctags" nil cmd)
       (lambda (_proc event)
         (when (string-match-p "finished" event)
           (visit-tags-table (expand-file-name "TAGS" root) t)
           (message "TAGS updated for %s" root)))))))

(defun +tim/ruby-ctags-on-save-h ()
  "Debounced TAGS regeneration on save. Waits 5s of idle time."
  (when +tim/ruby-ctags-timer
    (cancel-timer +tim/ruby-ctags-timer))
  (setq +tim/ruby-ctags-timer
        (run-with-idle-timer 5 nil #'+tim/ruby-ctags-generate)))

(defun +tim/ruby-etags-setup-h ()
  "Set up etags-based completion and xref for the current Ruby buffer."
  ;; Load project TAGS if it exists
  (when-let ((root (doom-project-root)))
    (let ((tags-file (expand-file-name "TAGS" root)))
      (when (file-exists-p tags-file)
        (visit-tags-table tags-file t))))

  ;; Add etags to completion-at-point
  (add-hook 'completion-at-point-functions #'tags-completion-at-point-function nil t)

  ;; Use etags as xref backend (makes SPC t d work with tags)
  (setq-local xref-backend-functions '(etags--xref-backend)))

;; Activate for Ruby buffers in Rails projects
(add-hook 'ruby-mode-hook
          (defun +tim/maybe-enable-ruby-etags-h ()
            (when (eq (ignore-errors (+tim/detect-framework (doom-project-root))) 'rails)
              (+tim/ruby-etags-setup-h)
              (add-hook 'after-save-hook #'+tim/ruby-ctags-on-save-h nil t))))

(add-hook 'ruby-ts-mode-hook #'+tim/maybe-enable-ruby-etags-h)

;; Add a manual generate binding under the tags prefix
(map! :leader
      (:prefix "t"
       :desc "Generate TAGS (Ruby)" "r" #'+tim/ruby-ctags-generate))

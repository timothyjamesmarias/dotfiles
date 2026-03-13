;;; Lem Configuration File
;;; Location: ~/dotfiles/lem/init.lisp
;;;
;;; This file is loaded when Lem starts.
;;; Symlink this to ~/.lem/init.lisp or ~/.config/lem/init.lisp

(in-package :lem-user)

;;; Theme configuration
;; (load-theme "dark")

;;; Key bindings
;; Example: bind C-c C-t to a custom command
;; (define-key *global-keymap* "C-c C-t" 'my-custom-command)

;;; Enable vi-mode globally (uncomment if desired)
;; (lem-vi-mode:vi-mode)

;;; Set default indentation
(setf (variable-value 'indent-tabs-mode :global) nil)  ; Use spaces instead of tabs
(setf (variable-value 'tab-width :global) 4)           ; Tab width of 4

;;; Enable line numbers
(line-numbers-mode t)

;;; Auto-save configuration
;; (setf (variable-value 'auto-save :global) t)

;;; Custom functions
(defun my-lem-setup ()
  "Personal Lem setup function"
  ;; Add your custom initialization here
  )

;; Run setup
(my-lem-setup)

;;; Language-specific configurations
;; Python mode
(add-hook *python-mode-hook*
          (lambda ()
            (setf (variable-value 'tab-width) 4)))

;; JavaScript/TypeScript
(add-hook *js-mode-hook*
          (lambda ()
            (setf (variable-value 'tab-width) 2)))

;;; Load additional configuration files if they exist
(let ((custom-file (merge-pathnames "custom.lisp"
                                    (merge-pathnames ".lem/" (user-homedir-pathname)))))
  (when (probe-file custom-file)
    (load custom-file)))
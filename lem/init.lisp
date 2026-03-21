;;; Lem Configuration File
(in-package :lem-user)

;;; Enable Vi mode
(lem-vi-mode:vi-mode)

;;; Enable frame multiplexer (for project workspaces)
(lem/frame-multiplexer:toggle-frame-multiplexer)

;;; Load perspectives system (from the same directory as this init.lisp)
(load (merge-pathnames "perspectives.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))

;;; Load sessionizer (from the same directory as this init.lisp)
(load (merge-pathnames "sessionizer.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))

;;; Keybindings
;; Project sessionizer (like tmux-sessionizer)
(define-key *global-keymap* "M-p" 'sessionizer)

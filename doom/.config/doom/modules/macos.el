;;; modules/macos.el --- macOS integration -*- lexical-binding: t; -*-

(defun +tim/--macos-require ()
  "Ensure we're on macOS with the `open' CLI available."
  (unless (eq system-type 'darwin)
    (user-error "macOS only"))
  (unless (executable-find "open")
    (user-error "`open' CLI not found on PATH")))

(defun +tim/--macos-list-apps ()
  "Return a list of installed .app bundle names for completion."
  (let ((dirs '("/Applications" "/System/Applications"
                "/Applications/Utilities" "/System/Applications/Utilities"))
        names)
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (f (directory-files dir nil "\\.app\\'" t))
          (push (file-name-sans-extension f) names))))
    (sort (delete-dups names) #'string<)))

(defun +tim/macos-open-with (app)
  "Open the current file (or dired file at point) with APP via `open -a'."
  (interactive
   (list (completing-read "Open with app: " (+tim/--macos-list-apps) nil nil)))
  (+tim/--macos-require)
  (+macos-open-with app))

(map! :leader
      (:prefix "o"
       :desc "Open in default app" "x" #'+macos/open-in-default-program
       :desc "Open with app…"      "w" #'+tim/macos-open-with))

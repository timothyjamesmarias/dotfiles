;;; modules/github.el --- GitHub CLI integration -*- lexical-binding: t; -*-

(defun +tim/--gh-project-root ()
  "Return project root or signal a user error."
  (or (and (fboundp 'doom-project-root) (doom-project-root))
      (locate-dominating-file default-directory ".git")
      (user-error "Not inside a git project")))

(defun +tim/--gh-require ()
  "Ensure the `gh' CLI is available on PATH."
  (unless (executable-find "gh")
    (user-error "`gh' CLI not found on PATH")))

(defun +tim/gh-pr-create (&optional arg)
  "Create a GitHub pull request for the current branch.
Default runs `gh pr create --fill --web' to open the PR draft in the
browser with title/body pre-filled from commits. With prefix ARG, run
`gh pr create' interactively in a compile buffer."
  (interactive "P")
  (+tim/--gh-require)
  (let ((default-directory (+tim/--gh-project-root)))
    (if arg
        (compile "gh pr create" t)
      (let ((buf (get-buffer-create "*gh pr create*")))
        (with-current-buffer buf (erase-buffer))
        (let ((code (call-process "gh" nil buf nil
                                  "pr" "create" "--fill" "--web")))
          (if (zerop code)
              (message "gh pr create: opened in browser")
            (pop-to-buffer buf)
            (user-error "gh pr create failed (exit %d)" code)))))))

(defun +tim/gh-browse ()
  "Open the current repo on GitHub. If visiting a file inside the repo,
open that file at the current line."
  (interactive)
  (+tim/--gh-require)
  (let* ((root (+tim/--gh-project-root))
         (default-directory root)
         (file (buffer-file-name))
         (target (when (and file (file-in-directory-p file root))
                   (format "%s:%d"
                           (file-relative-name file root)
                           (line-number-at-pos))))
         (args (if target (list "browse" target) (list "browse")))
         (code (apply #'call-process "gh" nil nil nil args)))
    (unless (zerop code)
      (user-error "gh browse failed (exit %d)" code))))

(defun +tim/gh-pr-view ()
  "Open the pull request for the current branch in the browser."
  (interactive)
  (+tim/--gh-require)
  (let* ((default-directory (+tim/--gh-project-root))
         (code (call-process "gh" nil nil nil "pr" "view" "--web")))
    (unless (zerop code)
      (user-error "gh pr view failed (exit %d) — is there a PR for this branch?" code))))

(map! :leader
      (:prefix ("g h" . "github")
       :desc "Create PR"            "p" #'+tim/gh-pr-create
       :desc "Open repo in browser" "o" #'+tim/gh-browse
       :desc "View PR in browser"   "v" #'+tim/gh-pr-view))

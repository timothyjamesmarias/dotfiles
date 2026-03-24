;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and

;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/notes/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `with-eval-after-load' block, otherwise Doom's defaults may override your
;; settings. E.g.
;;
;;   (with-eval-after-load 'PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look them up).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(setq native-comp-deferred-compilation nil)

(after! magit
  (setq magit-pull-or-fetch 'pull)
  (setq magit-rebase-arguments '("--autostash"))
  (setq magit-pull-arguments '("--rebase" "--autostash")))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(setq doom-theme 'modus-vivendi)
(setq doom-font (font-spec :family "MonoLisa" :size 14))
(setq-default line-spacing 4)

(defun new-note ()
  "Create a new note from template in org-directory."
  (interactive)
  (let* ((title (read-string "Note title: "))
         (slug (replace-regexp-in-string
                "[^a-z0-9]+" "-"
                (downcase title)))
         (slug (replace-regexp-in-string
                "^-\\|-$" "" slug))
         (filename (expand-file-name
                    (concat slug ".org") org-directory))
         (date (format-time-string "%Y-%m-%d")))
    (when (file-exists-p filename)
      (unless (y-or-n-p (format "%s exists. Open it? " filename))
        (user-error "Aborted")))
    (find-file filename)
    (when (= (buffer-size) 0)
      (insert (format "#+title: %s\n#+date: %s\n\n* Content\n" title date))
      (save-buffer))))

(defun project-ibuffer ()
  "Open ibuffer filtered to the current workspace's buffers."
  (interactive)
  (let ((bufs (+workspace-buffer-list)))
    (ibuffer nil (format "*ibuffer:%s*" (+workspace-current-name))
             `((predicate . (memq buf ',bufs))))))

;; --- Claude Code integration ---

(defun project-claude (n)
  "Open or switch to Claude Code session N for the current project."
  (interactive "p")
  (let* ((project (projectile-project-name))
         (buf-name (if (= n 1)
                       (format "*claude:%s*" project)
                     (format "*claude:%s<%d>*" project n)))
         (buf (get-buffer buf-name))
         (default-directory (projectile-project-root)))
    (if (and buf (buffer-live-p buf))
        (switch-to-buffer buf)
      (vterm buf-name)
      (vterm-send-string "claude\n"))))

(defun project-claude-continue ()
  "Open Claude Code continuing the last conversation in the current project."
  (interactive)
  (let* ((project (projectile-project-name))
         (buf-name (format "*claude:%s:continue*" project))
         (default-directory (projectile-project-root)))
    (vterm buf-name)
    (vterm-send-string "claude --continue\n")))

(defun project-claude-resume ()
  "Open Claude Code and pick a conversation to resume in the current project."
  (interactive)
  (let* ((project (projectile-project-name))
         (buf-name (format "*claude:%s:resume*" project))
         (default-directory (projectile-project-root)))
    (vterm buf-name)
    (vterm-send-string "claude --resume\n")))

(defun project-claude-list ()
  "Switch to a Claude Code buffer for the current project via completion."
  (interactive)
  (let* ((project (projectile-project-name))
         (prefix (format "*claude:%s" project))
         (bufs (seq-filter
                (lambda (b) (string-prefix-p prefix (buffer-name b)))
                (buffer-list))))
    (if bufs
        (switch-to-buffer
         (completing-read "Claude session: " (mapcar #'buffer-name bufs) nil t))
      (project-claude 1))))

(defun project-claude-new ()
  "Create a new Claude Code session with the next available number."
  (interactive)
  (let* ((project (projectile-project-name))
         (n 1))
    (while (get-buffer (if (= n 1)
                           (format "*claude:%s*" project)
                         (format "*claude:%s<%d>*" project n)))
      (setq n (1+ n)))
    (project-claude n)))

(defun claude-prompt ()
  "Send a one-shot prompt to Claude Code and display the result."
  (interactive)
  (let* ((prompt (read-string "Claude prompt: "))
         (default-directory (projectile-project-root))
         (buf (get-buffer-create "*claude-prompt*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Prompt: %s\n\n" prompt))
      (insert "Waiting for response...\n"))
    (display-buffer buf)
    (set-process-sentinel
     (start-process "claude-prompt" buf "claude" "-p" prompt)
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (with-current-buffer (process-buffer proc)
           (goto-char (point-min))
           (when (search-forward "Waiting for response...\n" nil t)
             (replace-match ""))
           (read-only-mode 1)
           (markdown-mode)))))))

(after! transient
  (transient-define-prefix project-claude-menu ()
    "Manage Claude Code sessions."
    [:description
     (lambda () (format "Claude: %s" (projectile-project-name)))
     ["Session"
      ("n" "New"       project-claude-new)
      ("c" "Continue"  project-claude-continue)
      ("r" "Resume"    project-claude-resume)
      ("l" "List"      project-claude-list)]
     ["Quick"
      ("p" "Prompt"    claude-prompt)]]))

(defun pandoc-convert ()
  "Convert current buffer between org and markdown via pandoc.
Creates a new file alongside the original."
  (interactive)
  (let* ((src (buffer-file-name))
         (ext (file-name-extension src))
         (base (file-name-sans-extension src))
         (target (pcase ext
                   ("org" (concat base ".md"))
                   ("md"  (concat base ".org"))
                   (_     (user-error "Not an org or md file"))))
         (fmt-from (pcase ext
                     ("org" "org")
                     ("md"  "markdown")))
         (fmt-to (pcase ext
                   ("org" "markdown")
                   ("md"  "org")
)))
    (when (and (file-exists-p target)
               (not (y-or-n-p (format "%s exists. Overwrite? " target))))
      (user-error "Aborted"))
    (if (zerop (call-process "pandoc" nil nil nil
                             "-f" fmt-from "-t" fmt-to
                             "-o" target src))
        (progn
          (message "Created %s" target)
          (find-file target))
      (user-error "Pandoc conversion failed"))))

;; --- Project workspace system ---

(defun sessionizer ()
  "Fuzzy-pick a project and switch to its workspace."
  (interactive)
  (let* ((dirs (append
                (directory-files "~/projects" t "^[^.]" t)
                (directory-files "~/notes" t "^[^.]" t)))
         (dirs (seq-filter #'file-directory-p dirs))
         (choice (completing-read "Switch to: " dirs nil t)))
    (when choice
      (projectile-add-known-project choice)
      (+workspace-switch (file-name-nondirectory choice) t)
      (setq default-directory choice)
      (doom-project-find-file choice))))

(defun project-terminal (n)
  "Open or switch to persistent terminal N for the current project.
With no prefix, opens terminal 1. C-u 2 opens terminal 2, etc."
  (interactive "p")
  (let* ((project (projectile-project-name))
         (buf-name (if (= n 1)
                       (format "*term:%s*" project)
                     (format "*term:%s<%d>*" project n)))
         (buf (get-buffer buf-name))
         (default-directory (projectile-project-root)))
    (if (and buf (buffer-live-p buf))
        (switch-to-buffer buf)
      (vterm buf-name))))

(defun project-terminal-list ()
  "Switch to a project terminal via completion."
  (interactive)
  (let* ((project (projectile-project-name))
         (prefix (format "*term:%s" project))
         (bufs (seq-filter
                (lambda (b) (string-prefix-p prefix (buffer-name b)))
                (buffer-list))))
    (if bufs
        (switch-to-buffer
         (completing-read "Terminal: " (mapcar #'buffer-name bufs) nil t))
      (project-terminal 1))))

(defun project-terminal-new ()
  "Create a new terminal for the current project with the next available number."
  (interactive)
  (let* ((project (projectile-project-name))
         (n 1))
    (while (get-buffer (if (= n 1)
                           (format "*term:%s*" project)
                         (format "*term:%s<%d>*" project n)))
      (setq n (1+ n)))
    (project-terminal n)))

(after! transient
  (transient-define-prefix project-terminal-menu ()
    "Manage project terminals."
    [:description
     (lambda () (format "Terminals: %s" (projectile-project-name)))
     ["Open"
      ("1" "Terminal 1" (lambda () (interactive) (project-terminal 1)))
      ("2" "Terminal 2" (lambda () (interactive) (project-terminal 2)))
      ("3" "Terminal 3" (lambda () (interactive) (project-terminal 3)))
      ("4" "Terminal 4" (lambda () (interactive) (project-terminal 4)))]
     ["Manage"
      ("n" "New"    project-terminal-new)
      ("l" "List"   project-terminal-list)
      ("p" "Popup"  +vterm/toggle)]])

  (transient-define-prefix project-hub ()
    "Project context hub."
    [:description
     (lambda () (format "Project: %s" (projectile-project-name)))
     ["Navigate"
      ("f" "Find file"    projectile-find-file)
      ("r" "Recent files" projectile-recentf)
      ("b" "Buffers"      projectile-switch-to-buffer)
      ("B" "Manage buffers" project-ibuffer)
      ("d" "Dired"        projectile-dired)
      ("s" "Search"       +default/search-project)
      ("n" "New note"     new-note)]
     ["Tools"
      ("t" "Terminals"    project-terminal-menu)
      ("g" "Magit"        magit-status)
      ("c" "Claude Code"  project-claude-menu)
      ("z" "Zen mode"     writeroom-mode)]]))

(map! :leader
      :desc "Jump to project" "o j" #'sessionizer
      :desc "Project hub" "p h" #'project-hub
      (:prefix ("n" . "notes")
       :desc "New note" "w" #'new-note))

(after! writeroom-mode
  (setq +zen-text-scale 0
        writeroom-width 80
        writeroom-mode-line t
        visual-fill-column-center-text t)
  (add-hook 'writeroom-mode-enable-hook
            (defun +zen-enable-word-wrap-h ()
              (visual-line-mode +1)
              (+word-wrap-mode +1)))
  (add-hook 'writeroom-mode-disable-hook
            (defun +zen-disable-word-wrap-h ()
              (visual-line-mode -1)
              (+word-wrap-mode -1))))

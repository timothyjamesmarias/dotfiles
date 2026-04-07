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

(defun term--get-or-create (n)
  "Get project terminal N buffer, creating if needed without disrupting windows."
  (let* ((project (projectile-project-name))
         (buf-name (if (= n 1)
                       (format "*term:%s*" project)
                     (format "*term:%s<%d>*" project n)))
         (buf (get-buffer buf-name)))
    (if (and buf (buffer-live-p buf))
        buf
      (let ((default-directory (projectile-project-root)))
        (save-window-excursion
          (vterm buf-name)
          (get-buffer buf-name))))))

;; --- Fast toggle between code and terminal ---

(defvar term--last-code-buffer nil
  "Last non-terminal buffer visited, for fast toggle.")

(defun term--track-last-code-buffer ()
  "Record the current buffer if it is not a terminal."
  (unless (derived-mode-p 'vterm-mode)
    (setq term--last-code-buffer (current-buffer))))

(add-hook 'buffer-list-update-hook #'term--track-last-code-buffer)

(defun term-fast-toggle ()
  "Toggle between the last code buffer and the most recent project terminal."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (when (and term--last-code-buffer
                 (buffer-live-p term--last-code-buffer))
        (switch-to-buffer term--last-code-buffer))
    (let* ((project (projectile-project-name))
           (prefix (format "*term:%s" project))
           (term-buf (cl-find-if
                      (lambda (b)
                        (and (string-prefix-p prefix (buffer-name b))
                             (buffer-live-p b)))
                      (buffer-list))))
      (if term-buf
          (switch-to-buffer term-buf)
        (project-terminal 1)))))

(map! :n "M-o" #'term-fast-toggle
      :i "M-o" #'term-fast-toggle)

(after! vterm
  (define-key vterm-mode-map (kbd "M-o") #'term-fast-toggle))

;; --- Terminal layouts ---

(defun term-popup-toggle ()
  "Toggle a persistent bottom terminal for the current project."
  (interactive)
  (let* ((term-buf (term--get-or-create 1))
         (win (get-buffer-window term-buf)))
    (if win
        (delete-window win)
      (display-buffer-in-side-window
       term-buf
       '((side . bottom)
         (slot . 0)
         (window-height . 0.3)))
      (select-window (get-buffer-window term-buf))
      (set-window-dedicated-p (selected-window) t))))

(defun term-layout-below ()
  "Split window with terminal in the bottom third."
  (interactive)
  (let ((term-buf (term--get-or-create 1)))
    (select-window (split-window-below (floor (* (window-height) 0.7))))
    (switch-to-buffer term-buf)
    (set-window-dedicated-p (selected-window) t)))

(defun term-layout-right ()
  "Split window with terminal on the right."
  (interactive)
  (let ((term-buf (term--get-or-create 1)))
    (select-window (split-window-right))
    (switch-to-buffer term-buf)
    (set-window-dedicated-p (selected-window) t)))

(defun term-layout-grid ()
  "Create a 2x2 grid of terminals 1-4."
  (interactive)
  (delete-other-windows)
  (let* ((b1 (term--get-or-create 1))
         (b2 (term--get-or-create 2))
         (b3 (term--get-or-create 3))
         (b4 (term--get-or-create 4))
         (w1 (selected-window))
         (w2 (split-window-right))
         (w3 (progn (select-window w1) (split-window-below)))
         (w4 (progn (select-window w2) (split-window-below))))
    (set-window-buffer w1 b1)
    (set-window-buffer w2 b2)
    (set-window-buffer w3 b3)
    (set-window-buffer w4 b4)
    (dolist (w (list w1 w2 w3 w4))
      (set-window-dedicated-p w t))
    (select-window w1)))

;; --- Broadcast ---

(defun term-broadcast (command)
  "Send COMMAND to all vterm buffers in the current project."
  (interactive "sCommand to broadcast: ")
  (let* ((project (projectile-project-name))
         (prefix (format "*term:%s" project))
         (bufs (seq-filter
                (lambda (b)
                  (and (string-prefix-p prefix (buffer-name b))
                       (buffer-live-p b)
                       (with-current-buffer b
                         (derived-mode-p 'vterm-mode))))
                (buffer-list)))
         (count 0))
    (if (null bufs)
        (message "No terminals for project %s" project)
      (dolist (buf bufs)
        (with-current-buffer buf
          (vterm-send-string command)
          (vterm-send-return)
          (setq count (1+ count))))
      (message "Sent to %d terminal(s)" count))))

;; --- Window dedication ---

(defun term-toggle-dedication ()
  "Toggle whether the current window is dedicated to its buffer."
  (interactive)
  (let ((dedicated (not (window-dedicated-p))))
    (set-window-dedicated-p (selected-window) dedicated)
    (message "Window %s" (if dedicated "dedicated" "undedicated"))))

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
      ("n" "New"       project-terminal-new)
      ("l" "List"      project-terminal-list)
      ("p" "Popup"     term-popup-toggle)]
     ["Layout"
      ("h" "Below"     term-layout-below)
      ("j" "Right"     term-layout-right)
      ("k" "Grid"      term-layout-grid)
      ("x" "Broadcast" term-broadcast)]])

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
      :desc "Window dedication" "w d" #'term-toggle-dedication
      (:prefix "t"
       :desc "Terminal 1"    "1" (cmd! (project-terminal 1))
       :desc "Terminal 2"    "2" (cmd! (project-terminal 2))
       :desc "Terminal 3"    "3" (cmd! (project-terminal 3))
       :desc "Terminal 4"    "4" (cmd! (project-terminal 4))
       :desc "New terminal"  "n" #'project-terminal-new
       :desc "List terminals" "a" #'project-terminal-list
       :desc "Popup toggle"  "t" #'term-popup-toggle
       :desc "Layout: below" "h" #'term-layout-below
       :desc "Layout: right" "j" #'term-layout-right
       :desc "Layout: grid"  "k" #'term-layout-grid
       :desc "Broadcast"     "x" #'term-broadcast
       :desc "Terminal menu" "e" #'project-terminal-menu)
      (:prefix ("n" . "notes")
       :desc "New note" "w" #'new-note))

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '(web-mode . "vue")))

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

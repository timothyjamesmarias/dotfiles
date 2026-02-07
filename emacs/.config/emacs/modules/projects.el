;;; projects.el --- Project management with Projectile -*- lexical-binding: t; -*-
;;
;; Description: Projectile configuration for project-based workflow
;;
;;; Commentary:
;; Projectile replaces your tmux-sessionizer workflow:
;;   tmux sessions → Projectile projects
;;   M-S sessionizer → M-S / SPC p p (projectile-switch-project)
;;
;; Searches for projects in ~/projects and ~ (like tmux-sessionizer)
;;
;;; Code:

;; ========================================
;; Projectile - Project Management
;; ========================================

(use-package projectile
  :demand t
  :init
  (projectile-mode +1)

  :config
  ;; Where to search for projects (matches your tmux-sessionizer)
  (setq projectile-project-search-path
        '(("~/projects" . 2)))  ; Search 2 levels deep in ~/projects
        ;; Removed ("~" . 1) to avoid .Trash and other system directories
        ;; Add specific directories if needed, e.g.: "~/code" "~/work"

  ;; Ignore problematic directories
  (setq projectile-globally-ignored-directories
        (append '(".git" ".svn" ".hg" "node_modules" ".Trash" "Library"
                  "Applications" "Downloads" "Desktop" "Documents"
                  "Pictures" "Music" "Movies" "Public")
                projectile-globally-ignored-directories))

  ;; Use ripgrep for faster project indexing (you already use rg extensively)
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-git-command "rg --files --hidden --follow --glob '!.git'"
        projectile-generic-command "rg --files --hidden --follow --glob '!.git'")

  ;; Sort files by recently active (like your workflow)
  (setq projectile-sort-order 'recentf)

  ;; Cache file for faster startup
  (setq projectile-cache-file
        (expand-file-name "projectile.cache" user-emacs-directory))

  ;; Known projects file
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

  ;; Completion system (use default - vertico will handle it)
  (setq projectile-completion-system 'default)

  ;; Switch project action (opens project root in dired/vterm/magit)
  (setq projectile-switch-project-action #'projectile-dired)

  ;; Don't ask to create project (auto-detect)
  (setq projectile-require-project-root nil)

  ;; Automatically discover projects on startup
  (projectile-discover-projects-in-search-path))

;; ========================================
;; Consult-Projectile - Better Integration
;; ========================================

(use-package consult-projectile
  :after (consult projectile)
  :config
  ;; Replace default projectile commands with consult versions
  (setq consult-projectile-use-projectile-switch-project t))

;; ========================================
;; Tab-Bar Mode - Multiple Project Workspaces
;; ========================================

;; Use tab-bar for managing multiple projects simultaneously
;; Like tmux windows but for project workspaces
(use-package tab-bar
  :straight nil  ; Built-in (Emacs 27+)
  :config
  (setq tab-bar-show 1)                    ; Show tabs if more than 1
  (setq tab-bar-close-button-show nil)     ; No close button
  (setq tab-bar-new-tab-choice "*scratch*") ; New tab shows scratch

  ;; Tab bar format (show tab number + name)
  (setq tab-bar-tab-hints t)               ; Show numbers
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

  ;; New tabs position
  (setq tab-bar-new-tab-to 'rightmost))

;; ========================================
;; Project Tab Integration
;; ========================================

;; Helper function to open project in a new tab
(defun tim/project-switch-project-new-tab ()
  "Switch to a project in a new tab."
  (interactive)
  (let ((project (projectile-acquire-root)))
    (tab-bar-new-tab)
    (tab-bar-rename-tab (file-name-nondirectory (directory-file-name project)))
    (projectile-switch-project-by-name project)))

;; Helper function to switch to project in current tab
(defun tim/project-switch-project ()
  "Switch to a project using consult-projectile with better UX."
  (interactive)
  (if (fboundp 'consult-projectile-switch-project)
      (consult-projectile-switch-project)
    (projectile-switch-project)))

;; ========================================
;; Perspective.el - Alternative to Tab-Bar
;; ========================================

;; Perspective provides named workspaces for projects
;; Lighter weight than tab-bar, more powerful isolation
;; Uncomment if you prefer this over tab-bar

;; (use-package perspective
;;   :demand t
;;   :config
;;   (persp-mode)
;;   (setq persp-mode-prefix-key (kbd "C-c M-p"))
;;   (setq persp-state-default-file
;;         (expand-file-name "perspective-state" user-emacs-directory)))

;; (use-package persp-projectile
;;   :after (perspective projectile)
;;   :config
;;   ;; Automatically switch perspective when switching projects
;;   (setq projectile-switch-project-action #'projectile-persp-switch-project))

;; ========================================
;; Keybindings
;; ========================================

(with-eval-after-load 'evil
  (with-eval-after-load 'general
    (when (fboundp 'tim/leader-keys)
      (tim/leader-keys
    ;; Project switching (like M-S tmux-sessionizer)
    "p" '(:ignore t :which-key "projects")
    "pp" '(tim/project-switch-project :which-key "switch project")
    "pP" '(tim/project-switch-project-new-tab :which-key "switch project (new tab)")
    "pa" '(projectile-add-known-project :which-key "add project")
    "pd" '(projectile-remove-known-project :which-key "remove project")
    "pD" '(projectile-discover-projects-in-search-path :which-key "discover projects")

    ;; Project files (like M-o ff but project-aware)
    "pf" '(projectile-find-file :which-key "find file")
    "pr" '(projectile-recentf :which-key "recent files")
    "pF" '(projectile-find-file-other-window :which-key "find file other window")

    ;; Project search (like M-r rgf but project-aware)
    "ps" '(consult-ripgrep :which-key "ripgrep")
    "pS" '(projectile-grep :which-key "grep")

    ;; Project directories
    "po" '(projectile-find-other-file :which-key "find other file")
    "pE" '(projectile-edit-dir-locals :which-key "edit dir locals")

    ;; Project buffers
    "pb" '(projectile-switch-to-buffer :which-key "switch buffer")
    "pk" '(projectile-kill-buffers :which-key "kill buffers")

    ;; Project commands
    "pc" '(projectile-compile-project :which-key "compile")
    "pR" '(projectile-run-project :which-key "run")
    "pT" '(projectile-test-project :which-key "test")

    ;; Tab bar bindings (M-1 through M-9 will be in keybindings.el)
    "TAB" '(:ignore t :which-key "tabs")
    "TAB TAB" '(tab-bar-switch-to-recent-tab :which-key "recent tab")
    "TAB n" '(tab-bar-new-tab :which-key "new tab")
    "TAB d" '(tab-bar-close-tab :which-key "close tab")
    "TAB r" '(tab-bar-rename-tab :which-key "rename tab")
    "TAB 1" '(tab-bar-select-tab-by-name :which-key "select tab")))))

;;; projects.el ends here

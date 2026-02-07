;;; git.el --- Git integration with Magit -*- lexical-binding: t; -*-
;;
;; Description: Magit and Git workflows
;;
;;; Commentary:
;; Magit replaces your extensive git.zsh workflows (600+ lines!)
;; The transient interface provides action menus similar to your
;; FZF-based git workflows.
;;
;; Key mappings from git.zsh → Magit:
;;   glog (history with FZF) → magit-log with transient menu
;;   ga (stage files) → magit-stage with multi-select
;;   gu (unstage) → magit-unstage
;;   gcmsg (commit) → magit-commit
;;   gfix (fixup) → magit-commit-fixup
;;   gsq (squash) → magit-rebase-interactive
;;   gss (search history) → magit-log-grep
;;
;;; Code:

;; ========================================
;; Magit - Git Porcelain
;; ========================================

(use-package magit
  :config
  ;; Performance tweaks for large repos
  (setq magit-refresh-status-buffer nil)
  (setq magit-revision-show-gravatars nil)

  ;; Better display of diffs
  (setq magit-diff-refine-hunk 'all)

  ;; Show word-level diff highlighting
  (setq magit-diff-refine-ignore-whitespace t)

  ;; Automatically show recent commits
  (setq magit-log-section-commit-count 10)

  ;; Show full commit messages in log
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

  ;; Clone default directory
  (setq magit-clone-default-directory "~/projects/")

  ;; Disable magit auto-save
  (setq magit-save-repository-buffers 'dontask)

  ;; Use delta for diffs (you already use git-delta in .gitconfig)
  (setq magit-delta-default-dark-theme "Dracula")

  ;; Split direction (prefer horizontal splits like tmux)
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

;; ========================================
;; Magit-Delta - Better Diffs with Delta
;; ========================================

(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

;; ========================================
;; Forge - GitHub/GitLab Integration
;; ========================================

;; Forge provides GitHub PR/Issue integration in Magit
;; Matches your extensive use of `gh` CLI
(use-package forge
  :after magit
  :config
  ;; Clone settings
  (setq forge-clone-default-directory "~/projects/")

  ;; Reduce initial data fetch
  (setq forge-pull-notifications nil))

;; ========================================
;; Git-Timemachine - Browse File History
;; ========================================

;; Replaces your gfh (file history) function
(use-package git-timemachine
  :config
  (setq git-timemachine-show-minibuffer-details t))

;; ========================================
;; Git-Gutter - Show Git Diff in Gutter
;; ========================================

(use-package git-gutter
  :config
  (global-git-gutter-mode +1)

  ;; Update git-gutter more frequently
  (setq git-gutter:update-interval 0.2)

  ;; Visual appearance
  (setq git-gutter:modified-sign "│")
  (setq git-gutter:added-sign "│")
  (setq git-gutter:deleted-sign "│"))

;; ========================================
;; Git-Messenger - Show Commit Message at Point
;; ========================================

(use-package git-messenger
  :config
  (setq git-messenger:show-detail t)
  (setq git-messenger:use-magit-popup t))

;; ========================================
;; Diff-hl - Alternative to git-gutter
;; ========================================

;; Uncomment if you prefer diff-hl over git-gutter
;; diff-hl integrates better with Magit

;; (use-package diff-hl
;;   :config
;;   (global-diff-hl-mode)
;;   (diff-hl-flydiff-mode)
;;   (diff-hl-margin-mode)
;;   :hook
;;   (magit-pre-refresh . diff-hl-magit-pre-refresh)
;;   (magit-post-refresh . diff-hl-magit-post-refresh))

;; ========================================
;; Custom Git Functions
;; ========================================

(defun tim/git-commit-with-claude ()
  "Generate commit message using Claude (like your gclaude function)."
  (interactive)
  (let ((diff (shell-command-to-string "git diff --cached")))
    (if (string-empty-p (string-trim diff))
        (message "No staged changes to commit")
      ;; Open vterm and let user run their gclaude workflow
      ;; Or integrate with Claude API here
      (message "Open terminal and run: gclaude"))))

(defun tim/git-browse-remote ()
  "Open current file/line on GitHub (like your gh browse workflow)."
  (interactive)
  (let ((remote-url (magit-get "remote.origin.url")))
    (if remote-url
        (browse-url-at-point)
      (message "No remote configured"))))

(defun tim/magit-stage-all-and-commit ()
  "Stage all changes and commit (like your gcmsg workflow)."
  (interactive)
  (magit-stage-modified)
  (magit-commit-create))

;; ========================================
;; Keybindings
;; ========================================

(with-eval-after-load 'evil
  (with-eval-after-load 'general
    (when (fboundp 'tim/leader-keys)
      (tim/leader-keys
    ;; Git prefix (g)
    "g" '(:ignore t :which-key "git")

    ;; Status and basic operations
    "gg" '(magit-status :which-key "status")
    "gG" '(magit-dispatch :which-key "dispatch")
    "gi" '(magit-init :which-key "init")
    "gc" '(magit-clone :which-key "clone")

    ;; Staging (like ga, gap, gu)
    "gs" '(magit-stage-file :which-key "stage file")
    "gS" '(magit-stage-modified :which-key "stage all modified")
    "gu" '(magit-unstage-file :which-key "unstage file")
    "gU" '(magit-unstage-all :which-key "unstage all")

    ;; Committing (like gcmsg, gfix)
    "gC" '(:ignore t :which-key "commit")
    "gCc" '(magit-commit-create :which-key "commit")
    "gCa" '(magit-commit-amend :which-key "amend")
    "gCe" '(magit-commit-extend :which-key "extend")
    "gCf" '(magit-commit-fixup :which-key "fixup")
    "gCs" '(magit-commit-squash :which-key "squash")

    ;; Branching
    "gb" '(:ignore t :which-key "branch")
    "gbb" '(magit-branch-checkout :which-key "checkout")
    "gbc" '(magit-branch-create :which-key "create")
    "gbd" '(magit-branch-delete :which-key "delete")
    "gbr" '(magit-branch-rename :which-key "rename")

    ;; Fetching/Pulling/Pushing
    "gf" '(magit-fetch :which-key "fetch")
    "gF" '(magit-pull :which-key "pull")
    "gp" '(magit-push :which-key "push")
    "gP" '(magit-push-current :which-key "push current")

    ;; Logging/History (like glog, gfh, gss)
    "gl" '(:ignore t :which-key "log")
    "gll" '(magit-log-current :which-key "log current")
    "glf" '(magit-log-buffer-file :which-key "log file")
    "glb" '(magit-log-all-branches :which-key "log all branches")
    "gls" '(magit-log-grep :which-key "search log")

    ;; Diffing
    "gd" '(:ignore t :which-key "diff")
    "gdd" '(magit-diff-unstaged :which-key "diff unstaged")
    "gds" '(magit-diff-staged :which-key "diff staged")
    "gdc" '(magit-diff-buffer-file :which-key "diff file")
    "gdr" '(magit-diff-range :which-key "diff range")

    ;; Rebasing (like grba, gsq)
    "gr" '(:ignore t :which-key "rebase")
    "grr" '(magit-rebase :which-key "rebase")
    "gri" '(magit-rebase-interactive :which-key "rebase interactive")
    "grc" '(magit-rebase-continue :which-key "continue")
    "gra" '(magit-rebase-abort :which-key "abort")

    ;; Stashing
    "gz" '(:ignore t :which-key "stash")
    "gzz" '(magit-stash :which-key "stash")
    "gzp" '(magit-stash-pop :which-key "pop")
    "gzl" '(magit-stash-list :which-key "list")

    ;; Worktrees (like gwta, gwts)
    "gw" '(:ignore t :which-key "worktree")
    "gwc" '(magit-worktree-checkout :which-key "checkout")
    "gws" '(magit-worktree-status :which-key "status")
    "gwd" '(magit-worktree-delete :which-key "delete")

    ;; File history & blame
    "gt" '(git-timemachine :which-key "timemachine")
    "gB" '(magit-blame :which-key "blame")
    "gm" '(git-messenger:popup-message :which-key "show commit")

    ;; Forge (GitHub/GitLab)
    "gH" '(:ignore t :which-key "forge")
    "gHi" '(forge-visit-issue :which-key "visit issue")
    "gHp" '(forge-visit-pullreq :which-key "visit PR")
    "gHc" '(forge-create-pullreq :which-key "create PR")
    "gHl" '(forge-list-issues :which-key "list issues")
    "gHL" '(forge-list-pullreqs :which-key "list PRs")

    ;; Git gutter navigation
    "gn" '(git-gutter:next-hunk :which-key "next hunk")
    "gN" '(git-gutter:previous-hunk :which-key "prev hunk")
    "gv" '(git-gutter:popup-hunk :which-key "show hunk")
    "gR" '(git-gutter:revert-hunk :which-key "revert hunk")

    ;; Custom functions
    "gx" '(tim/git-browse-remote :which-key "browse remote")))))

;; ========================================
;; Evil Integration for Magit
;; ========================================

;; evil-collection already provides good bindings, but add some extras
(with-eval-after-load 'magit
  ;; Use j/k for navigation in magit buffers (already done by evil-collection)
  ;; But ensure tab opens/closes sections
  (define-key magit-mode-map (kbd "TAB") 'magit-section-toggle)
  (define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle))

;;; git.el ends here

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
(setq doom-theme 'doom-material-dark)
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

(after! vterm
  (define-key vterm-mode-map (kbd "M-o") #'term-fast-toggle))

(map! :leader
      (:prefix ("n" . "notes")
       :desc "New note" "w" #'new-note))

(defun +tim/project-workspace-buffers ()
  "Buffers that belong to both the current project and current workspace.
Falls back gracefully when one or both scopes are unavailable."
  (let* ((in-project (and (fboundp 'projectile-project-p)
                          (projectile-project-p)))
         (proj (and in-project (projectile-project-buffers)))
         (persp (and (bound-and-true-p persp-mode)
                     (fboundp 'persp-buffer-list)
                     (persp-buffer-list))))
    (cond
     ((and proj persp) (seq-intersection proj persp))
     (proj proj)
     (persp persp)
     (t (buffer-list)))))

(defun +tim/cycle-buffer (direction)
  "Cycle to the next (1) or previous (-1) project×workspace buffer."
  (let* ((bufs (+tim/project-workspace-buffers))
         (bufs (seq-filter
                (lambda (b)
                  (and (not (string-prefix-p " " (buffer-name b)))
                       (not (with-current-buffer b
                              (derived-mode-p 'vterm-mode
                                              'term-mode
                                              'eshell-mode
                                              'shell-mode
                                              'comint-mode
                                              'magit-mode)))
                       (if (fboundp 'doom-real-buffer-p)
                           (doom-real-buffer-p b)
                         t)))
                bufs)))
    (if (or (null bufs) (<= (length bufs) 1))
        (if (> direction 0) (next-buffer) (previous-buffer))
      (let* ((cur (current-buffer))
             (idx (or (seq-position bufs cur) 0))
             (nxt (mod (+ idx direction) (length bufs))))
        (switch-to-buffer (nth nxt bufs))))))

(defun +tim/next-project-workspace-buffer ()
  "Cycle to the next buffer in the current project ∩ workspace."
  (interactive) (+tim/cycle-buffer 1))

(defun +tim/previous-project-workspace-buffer ()
  "Cycle to the previous buffer in the current project ∩ workspace."
  (interactive) (+tim/cycle-buffer -1))

(map! :n "C-n" #'+tim/next-project-workspace-buffer
      :n "C-p" #'+tim/previous-project-workspace-buffer)

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

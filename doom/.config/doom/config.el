;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; --- Settings ---
(setq org-directory "~/notes/")
(setq native-comp-deferred-compilation nil)
(setq display-line-numbers-type 'relative)
(setq doom-theme 'doom-tomorrow-night)
(setq doom-font (font-spec :family "MonoLisa" :size 14))
(setq-default line-spacing 4)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(global-display-line-numbers-mode)
(setq fancy-splash-image "~/.config/doom/emacs-e-logo.png")

;; --- Evil ---
(after! evil
  (define-key evil-insert-state-map (kbd "C-h") #'delete-backward-char)
  (define-key evil-ex-completion-map (kbd "C-h") #'delete-backward-char)
  (define-key evil-ex-search-keymap (kbd "C-h") #'delete-backward-char))

;; --- Magit ---
(after! magit
  (setq magit-pull-or-fetch 'pull)
  (setq magit-rebase-arguments '("--autostash"))
  (setq magit-pull-arguments '("--rebase" "--autostash")))

;; --- Vterm ---
(after! vterm
  (define-key vterm-mode-map (kbd "M-o") #'term-fast-toggle)

  (defun tim/vterm-dnd--paths (data)
    "Return a list of file path strings from a drag-n-drop event DATA."
    (cond
     ((stringp data) (list data))
     ((and (consp data) (symbolp (car data)) (stringp (cadr data)))
      (list (cadr data)))
     ((and (consp data) (consp (car data)))
      (mapcar (lambda (x) (if (consp x) (or (cdr-safe x) (cadr x)) x)) data))
     ((listp data)
      (seq-filter #'stringp data))))

  (defun tim/vterm-dnd (event)
    "Insert a dropped file's path at the vterm prompt instead of opening it."
    (interactive "e")
    (dolist (f (tim/vterm-dnd--paths (nth 2 event)))
      (let ((path (if (string-match "\\`file://" f)
                      (url-unhex-string (substring f (match-end 0)))
                    f)))
        (vterm-insert (concat (shell-quote-argument path) " ")))))

  (define-key vterm-mode-map [drag-n-drop] #'tim/vterm-dnd)
  (define-key vterm-mode-map [M-drag-n-drop] #'tim/vterm-dnd)
  (define-key vterm-mode-map [s-drag-n-drop] #'tim/vterm-dnd))

;; --- Tags ---
(defun +tim/tag-find-all ()
  "Pick between dumb-jump definition, rg usages, tags, and xref history."
  (interactive)
  (let* ((choices '(("Definition (lookup chain)" . +lookup/definition)
                    ("Usages (ripgrep)"          . +default/search-project-for-symbol-at-point)
                    ("All tags"                  . projectile-find-tag)
                    ("Jump history"              . xref-go-back)))
         (pick (completing-read "Find: " (mapcar #'car choices) nil t)))
    (call-interactively (cdr (assoc pick choices)))))

(map! :leader
      (:prefix ("t" . "tags")
       :desc "Definition at point" "d" #'+lookup/definition
       :desc "Usages (rg)"         "u" #'+default/search-project-for-symbol-at-point
       :desc "Browse all tags"     "g" #'projectile-find-tag
       :desc "Jump history"        "s" #'xref-go-back
       :desc "Find all"            "a" #'+tim/tag-find-all))

;; --- Writeroom ---
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

;; --- Custom modules ---
(load! "modules/buffers")
(load! "modules/docker")
(load! "modules/files")
(load! "modules/github")
(load! "modules/macos")
(load! "modules/maizzle")
(load! "modules/notes")
(load! "modules/framework-detect")
(load! "modules/rails-nav")

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(recentf-mode 1)

(setq history-length 25)
(savehist-mode 1)
(save-place-mode 1)
(setq use-dialog-box nil)
(global-auto-revert-mode 1)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(set-face-attribute 'default nil :family "VictorMono Nerd Font Mono" :height 170)
(setq-default line-spacing 10)
(setq redisplay-dont-pause t
      scroll-margin 4
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(electric-pair-mode 1)
(setq tab-bar-mode 1)

(setq dired-dwim-target t)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package undo-tree)

(use-package doom-themes
  ;; :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (load-theme 'doom-dark+ t)
  ;; (setq doom-themes-treemacs-theme "doom-dark+")
  ;; (doom-themes-treemacs-config)
  ;; (doom-themes-org-config)
  )

(use-package doom-modeline
  ;; :ensure t
  :config
  (setq doom-modeline-height 40)
  :init (doom-modeline-mode 1))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after (counsel ivy)
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (ivy-prescient-mode 1))

(use-package general
  :after evil
  :defer t
  :config
  (general-evil-setup)
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-nmap
    "C-c 3" compile-command)

  (efs/leader-keys
    "tt" '(eshell :which-key "choose theme")
    "th" '(counsel-load-theme :which-key "choose theme")
    "w" '(save-buffer :which-key "save buffer")
    "q" '(kill-this-buffer :which-key "kill buffer")
    "g" '(magit-status :which-key "open magit")
    "fb" '(counsel-switch-buffer :which-key "find open buffers")
    "ff" '(projectile-find-file-dwim :which-key "find file in project")
    "vv" '(split-window-vertically :which-key "split vertically")
    "hh" '(split-window-horizontally :which-key "split horizontally")
    "nn" '(treemacs :which-key "open treemacs")
    "sf" '(evil-search :which-key "search file")
    ))

(use-package projectile
  :defer t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))

  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq
     projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :defer t
  :after (projectile counsel)
  :config (counsel-projectile-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(evil-mode 1)

(use-package evil-collection
  :after evil
  :defer t
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

; run M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (setq treemacs-width 60)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
	(treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
		    (not (null treemacs-python-executable)))
	(`(t . t)
	(treemacs-git-mode 'deferred))
	(`(t . _)
	(treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  ("C-x k" . persp-kill-buffer*)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package key-chord)
(key-chord-mode 1)

(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

; LSP stuff
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ivy
  :after lsp-mode)

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(global-company-mode 1)

(use-package company-box
  :hook (company-mode . company-box-mode))

; JS, TS, Vue packages
(use-package web-mode)

; Go packages
(setenv "PATH" (concat (getenv "PATH") ":/Users/timmarias/go"))
(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
	 (go-mode . company-mode))
  :bind (:map go-mode-map
	      ("C-c 4" . gofmt))
  :config
  (require 'lsp-go)
  (setq lsp-go-analyses
	'((fieldalignment . t)
	  (nilness . t)
	  (unusedwrite . t)
	  (unusedparams . t)))
  (add-to-list 'exec-path "~/go/bin")
  (setq gofmt-command "goimports"))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(add-hook 'python-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'go-mode-hook #'lsp)
(add-hook 'cpp-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)

; DAP stuff
(use-package dap-mode
  :commands dap-debug)

; Org mode
(use-package org
  :defer t)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("60d142f405a0bd2b653e2fea70b2ac80b0a2e5d55405a57e4c59df767fe0a45c" "97ef2fe48a437ea2e734556d5acf4c08c74647c497a952c1ae8571a71369f7a7" default))
 '(package-selected-packages
   '(perspective treemacs-tab-bar treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil doom-modeline dap-mode dirvish doom-themes go-mode web-mode lsp-ivy company-box company typescript-mode lsp-mode robe forge exec-path-from-shell evil-magit magit counsel-projectile projectile-rails projectile catppuccin-theme catpuccin-theme ivy-prescient counsel ivy-rich zenburn-theme undo-tree evil-commentary general all-the-icons ivy command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq ring-bell-function 'ignore)

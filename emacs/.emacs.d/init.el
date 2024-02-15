(setq inhibit-startup-message t)
(tool-bar-mode -1)

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
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  ;; (doom-themes-neotree-config)
  ;; (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(add-to-list 'load-path "~/.emacs.d/doom-moonfly-theme.el")
(require 'doom-moonfly-theme)
(load-theme 'doom-moonfly)

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
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package general
  :after evil
  :config
  (general-create-definer efs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (efs/leader-keys
    "tt" '(eshell :which-key "choose theme")
    "th" '(counsel-load-theme :which-key "choose theme")
    "w" '(save-buffer :which-key "save buffer")
    "q" '(kill-this-buffer :which-key "kill buffer")
    "g" '(magit-status :which-key "open magit")
    "fb" '(counsel-switch-buffer :which-key "find open buffers")
    "ff" '(projectile-find-file :which-key "find file in project")
    ))

(use-package dirvish
  :ensure t
  :config
  (dirvish-override-dired-mode))

(use-package projectile
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
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

; run M-x all-the-icons-install-fonts
(use-package all-the-icons)


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
(use-package vue-mode)

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
(use-package dap-mode)



; Org mode

(use-package org)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("60d142f405a0bd2b653e2fea70b2ac80b0a2e5d55405a57e4c59df767fe0a45c" "97ef2fe48a437ea2e734556d5acf4c08c74647c497a952c1ae8571a71369f7a7" default))
 '(package-selected-packages
   '(dap-mode dirvish doom-themes go-mode web-mode lsp-ivy company-box company typescript-mode lsp-mode robe forge exec-path-from-shell evil-magit magit counsel-projectile projectile-rails projectile catppuccin-theme catpuccin-theme ivy-prescient counsel ivy-rich zenburn-theme undo-tree evil-commentary general all-the-icons ivy command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq ring-bell-function 'ignore)

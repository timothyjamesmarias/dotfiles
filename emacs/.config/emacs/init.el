;;; init.el --- Main configuration entry point -*- lexical-binding: t; -*-
;;
;; Author: Tim Marias
;; Description: Modular Emacs configuration orchestrator
;;
;; This file loads individual modules, similar to how .zshrc sources
;; individual zsh configuration files. Each module is self-contained
;; and focused on a specific domain (evil, git, completion, etc.).
;;
;;; Commentary:
;;
;; Configuration Structure:
;;   early-init.el          - Performance & package bootstrap
;;   init.el (this file)    - Module orchestrator
;;   modules/
;;     core.el              - Basic Emacs settings
;;     evil.el              - Vim keybindings
;;     completion.el        - Vertico/Consult/Embark stack
;;     projects.el          - Projectile configuration
;;     vterm.el             - Terminal integration
;;     git.el               - Magit & Git workflows
;;     navigation.el        - Code navigation (dumb-jump, etags)
;;     docs.el              - Org-mode & Markdown
;;     keybindings.el       - Global keybindings (tmux M-* migration)
;;     ui.el                - Visual settings, which-key, theme
;;
;;; Code:

;; ========================================
;; Module Loading Helper
;; ========================================

(defun tim/load-module (module-name)
  "Load MODULE-NAME from the modules directory.
MODULE-NAME should be a string like 'core' or 'evil'."
  (let ((module-file (expand-file-name
                      (concat "modules/" module-name ".el")
                      user-emacs-directory)))
    (if (file-exists-p module-file)
        (progn
          (message "Loading module: %s" module-name)
          (load module-file nil 'nomessage))
      (message "Warning: Module not found: %s" module-name))))

;; ========================================
;; Load Modules in Order
;; ========================================

;; Core settings must load first
(tim/load-module "core")

;; UI and visual settings
(tim/load-module "ui")

;; Evil mode (Vim keybindings) - load early so other packages can integrate
(tim/load-module "evil")

;; Completion framework (Vertico/Consult/Embark)
(tim/load-module "completion")

;; Project management (Projectile)
(tim/load-module "projects")

;; Terminal integration (vterm)
(tim/load-module "vterm")

;; Git workflows (Magit)
(tim/load-module "git")

;; Code navigation (dumb-jump, etags, xref)
(tim/load-module "navigation")

;; Documentation tools (org-mode, markdown)
(tim/load-module "docs")

;; Global keybindings (load last to ensure all packages are available)
(tim/load-module "keybindings")

;; ========================================
;; Post-Initialization
;; ========================================

;; Show startup time in messages
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; ========================================
;; Custom File
;; ========================================

;; Keep custom-set-variables out of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here

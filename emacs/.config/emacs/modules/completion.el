;;; completion.el --- Vertico/Consult/Embark completion stack -*- lexical-binding: t; -*-
;;
;; Description: FZF-like completion framework
;;
;;; Commentary:
;; Vertico + Consult + Embark provides a modern, FZF-like completion
;; experience similar to your current shell workflow.
;;
;; Vertico:     Vertical completion UI (like FZF interface)
;; Consult:     Enhanced commands (search, buffer switch, ripgrep, etc.)
;; Embark:      Action menus (like your git action prompts)
;; Marginalia:  Rich annotations (file sizes, dates, docstrings)
;; Orderless:   Flexible fuzzy matching
;;
;;; Code:

;; ========================================
;; Vertico - Vertical Completion UI
;; ========================================

(use-package vertico
  :init
  (vertico-mode)
  :config
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates (like FZF's larger preview)
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Cycle back to top/bottom
  (setq vertico-cycle t))

;; ========================================
;; Orderless - Flexible Matching
;; ========================================

(use-package orderless
  :config
  ;; Configure orderless completion style
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; ========================================
;; Marginalia - Rich Annotations
;; ========================================

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode)
  :config
  ;; Align annotations to the right
  (setq marginalia-align 'right))

;; ========================================
;; Consult - Enhanced Commands
;; ========================================

(use-package consult
  :demand t
  :config
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure preview settings
  (setq consult-preview-key 'any) ; Preview as you navigate

  ;; Narrowing key
  (setq consult-narrow-key "<")

  ;; Configure ripgrep command (you already use rg heavily)
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 \
--path-separator / --smart-case --no-heading --with-filename \
--line-number --search-zip --hidden")

  ;; Configure project root function (will work with projectile)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

;; ========================================
;; Embark - Action Menu System
;; ========================================

(use-package embark
  :config
  ;; Replace default completions with embark-prefix-help-command
  (setq prefix-help-command #'embark-prefix-help-command)

  :general
  ;; Bind embark actions (like your git action menus)
  (:states '(normal visual insert emacs)
   "C-." 'embark-act         ; Pick an action using which-key
   "C-;" 'embark-dwim))      ; Do the default action

;; ========================================
;; Embark-Consult Integration
;; ========================================

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ========================================
;; Corfu - In-buffer Completion (optional)
;; ========================================

;; Corfu provides in-buffer completion popup (like Company mode)
;; Useful for code completion even without LSP (dumb completion works)
(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-cycle t)           ; Cycle through candidates
  (setq corfu-auto t)            ; Enable auto completion
  (setq corfu-auto-delay 0.2)    ; Delay before showing completions
  (setq corfu-auto-prefix 2)     ; Show after 2 characters
  (setq corfu-quit-no-match t)   ; Quit if no match
  (setq corfu-preview-current nil)
  (setq corfu-preselect 'prompt)

  ;; Enable Corfu in the minibuffer (but only if not using Vertico)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-auto nil) ; Disable auto completion in minibuffer
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

;; ========================================
;; Cape - Completion At Point Extensions
;; ========================================

;; Cape provides additional completion-at-point backends
;; Useful for file paths, dabbrev, keywords, etc. (no LSP needed)
(use-package cape
  :config
  ;; Add useful completions to completion-at-point
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; ========================================
;; Savehist - Save Minibuffer History
;; ========================================

(use-package savehist
  :straight nil  ; Built-in
  :init
  (savehist-mode)
  :config
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

;; ========================================
;; Enhanced Keybindings with General
;; ========================================

(with-eval-after-load 'evil
  (with-eval-after-load 'general
    (when (fboundp 'tim/leader-keys)
      (tim/leader-keys
    ;; Buffer/File search (enhanced with Consult)
    "bb" '(consult-buffer :which-key "switch buffer")
    "fr" '(consult-recent-file :which-key "recent files")

    ;; Search commands
    "ss" '(consult-line :which-key "search buffer")
    "si" '(consult-imenu :which-key "search imenu")
    "sI" '(consult-imenu-multi :which-key "search imenu all buffers")
    "so" '(consult-outline :which-key "search outline")
    "sk" '(consult-keep-lines :which-key "keep lines matching")
    "sf" '(consult-focus-lines :which-key "focus lines matching")

    ;; Jump commands
    "jj" '(consult-mark :which-key "jump to mark")
    "jJ" '(consult-global-mark :which-key "jump to global mark")
    "jl" '(consult-goto-line :which-key "jump to line")
    "ji" '(consult-imenu :which-key "jump to imenu item")

    ;; Register commands
    "rr" '(consult-register :which-key "registers")
    "rs" '(consult-register-store :which-key "store register")
    "rl" '(consult-register-load :which-key "load register")))))

;;; completion.el ends here

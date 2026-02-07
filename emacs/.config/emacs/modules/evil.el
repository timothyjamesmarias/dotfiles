;;; evil.el --- Vim keybindings configuration -*- lexical-binding: t; -*-
;;
;; Description: Evil mode (Vim emulation) and related packages
;;
;;; Commentary:
;; Configures comprehensive Vim keybindings to preserve your Vim muscle
;; memory from IdeaVim (937 lines of keybindings!) and Neovim.
;;
;; Leader key: SPC (in normal state)
;; Local leader: SPC m (for mode-specific commands)
;;
;;; Code:

;; ========================================
;; Evil - Vim Keybindings
;; ========================================

(use-package evil
  :init
  ;; Settings that must be set before evil loads
  (setq evil-want-integration t
        evil-want-keybinding nil   ; Let evil-collection handle keybindings
        evil-want-C-u-scroll t     ; C-u scrolls up (like Vim)
        evil-want-C-d-scroll t     ; C-d scrolls down (like Vim)
        evil-want-C-i-jump t       ; C-i for jump forward
        evil-undo-system 'undo-redo ; Use Emacs 28+ undo-redo system
        evil-split-window-below t  ; Like vim splitbelow
        evil-vsplit-window-right t ; Like vim splitright
        evil-respect-visual-line-mode t)

  :config
  ;; Enable evil mode globally
  (evil-mode 1)

  ;; Use visual line motions even when lines wrap
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Make horizontal movement cross lines (like in IdeaVim)
  (setq-default evil-cross-lines t)

  ;; Cursor shapes for different states (like in terminal vim)
  (setq evil-normal-state-cursor '(box "orange")
        evil-insert-state-cursor '(bar "green")
        evil-visual-state-cursor '(box "yellow")
        evil-replace-state-cursor '(hollow "red")
        evil-operator-state-cursor '(hollow "orange"))

  ;; Use Emacs state in certain modes (where Vim keybindings don't make sense)
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Use insert state in these modes
  (dolist (mode '(vterm-mode))
    (evil-set-initial-state mode 'emacs)))

;; ========================================
;; Evil Collection - Evil bindings for many modes
;; ========================================

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list
        '(magit
          dired
          help
          ibuffer
          (package-menu package)
          bookmark
          calc
          calendar
          compile
          consult
          custom
          diff-mode
          ediff
          eldoc
          embark
          grep
          info
          occur
          outline
          replace
          shortdoc
          vertico
          xref))
  (evil-collection-init))

;; ========================================
;; Evil Commentary - gc to comment
;; ========================================

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; ========================================
;; Evil Surround - Manipulate surrounding chars
;; ========================================

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; ========================================
;; Evil Escape - Better jk/kj escape
;; ========================================

(use-package evil-escape
  :after evil
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.2))

;; ========================================
;; General - Leader Key Framework
;; ========================================

(use-package general
  :after evil
  :config
  ;; Set SPC as the leader key (like Spacemacs/Doom/your IdeaVim setup)
  (general-create-definer tim/leader-keys
    :states '(normal visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC") ; C-SPC also works in insert/emacs states

  ;; Set SPC m as the local leader (mode-specific commands)
  (general-create-definer tim/local-leader-keys
    :states '(normal visual emacs)
    :prefix "SPC m"
    :global-prefix "C-SPC m")

  ;; Define leader key groups (like which-key groups in IdeaVim)
  (tim/leader-keys
    "SPC" '(execute-extended-command :which-key "M-x")
    ":" '(eval-expression :which-key "eval")

    ;; File operations (f prefix - like your IdeaVim <leader>f)
    "f" '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fr" '(recentf-open-files :which-key "recent files")
    "fs" '(save-buffer :which-key "save file")
    "fS" '(write-file :which-key "save as")
    "fd" '(dired :which-key "dired")

    ;; Buffer operations (b prefix)
    "b" '(:ignore t :which-key "buffers")
    "bb" '(switch-to-buffer :which-key "switch buffer")
    "bd" '(kill-current-buffer :which-key "delete buffer")
    "bk" '(kill-buffer :which-key "kill buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "br" '(revert-buffer :which-key "revert buffer")

    ;; Window operations (w prefix)
    "w" '(:ignore t :which-key "windows")
    "ww" '(other-window :which-key "other window")
    "wd" '(delete-window :which-key "delete window")
    "wo" '(delete-other-windows :which-key "delete others")
    "ws" '(split-window-below :which-key "split below")
    "wv" '(split-window-right :which-key "split right")
    "w=" '(balance-windows :which-key "balance windows")

    ;; Quit/Session (q prefix)
    "q" '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-emacs :which-key "quit emacs")
    "qr" '(restart-emacs :which-key "restart emacs")

    ;; Toggle operations (t prefix)
    "t" '(:ignore t :which-key "toggle")
    "tl" '(display-line-numbers-mode :which-key "line numbers")
    "tw" '(whitespace-mode :which-key "whitespace")
    "tv" '(visual-line-mode :which-key "visual line")

    ;; Help/Describe (h prefix)
    "h" '(:ignore t :which-key "help")
    "hf" '(describe-function :which-key "describe function")
    "hv" '(describe-variable :which-key "describe variable")
    "hk" '(describe-key :which-key "describe key")
    "hm" '(describe-mode :which-key "describe mode")
    "hp" '(describe-package :which-key "describe package")

    ;; Search (s prefix - will be enhanced by projects.el for ripgrep)
    "s" '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "search buffer")
    "si" '(consult-imenu :which-key "search imenu")

    ;; Project operations (p prefix - will be defined in projects.el)
    "p" '(:ignore t :which-key "projects")

    ;; Git operations (g prefix - will be defined in git.el)
    "g" '(:ignore t :which-key "git")

    ;; Open/Launch (o prefix - for terminals, etc.)
    "o" '(:ignore t :which-key "open")

    ;; Code operations (c prefix - for navigation, refactoring when applicable)
    "c" '(:ignore t :which-key "code")))

;; ========================================
;; Bindings for Non-Leader Keys
;; ========================================

;; Make ESC quit prompts (like in Vim)
(general-define-key
 :keymaps 'key-translation-map
 "ESC" (kbd "C-g"))

;; Better window navigation (C-h/j/k/l)
(general-define-key
 :states '(normal visual emacs)
 "C-h" 'evil-window-left
 "C-j" 'evil-window-down
 "C-k" 'evil-window-up
 "C-l" 'evil-window-right)

;; ========================================
;; Evil Insert-Mode Keybindings in Minibuffer
;; ========================================

;; Add useful evil insert-mode keybindings to minibuffer
;; This gives you vim-like editing without the complexity of modes
(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-u") 'backward-kill-sentence)
(define-key minibuffer-local-map (kbd "C-a") 'beginning-of-line)
(define-key minibuffer-local-map (kbd "C-e") 'end-of-line)

;; Vertico-specific keybindings
(with-eval-after-load 'vertico
  ;; Editing keys
  (define-key vertico-map (kbd "C-w") 'backward-kill-word)
  (define-key vertico-map (kbd "C-u") 'backward-kill-sentence)

  ;; Navigation (C-j/C-k for next/previous like vim)
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "M-j") 'vertico-next)
  (define-key vertico-map (kbd "M-k") 'vertico-previous))

;;; evil.el ends here

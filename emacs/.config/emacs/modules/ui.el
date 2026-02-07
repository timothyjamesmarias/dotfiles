;;; ui.el --- Visual configuration and enhancements -*- lexical-binding: t; -*-
;;
;; Description: Theme, mode-line, which-key, and visual tweaks
;;
;;; Commentary:
;; Visual configuration for a clean, productive Emacs environment.
;; You already use which-key in IdeaVim, so this will feel familiar!
;;
;;; Code:

;; ========================================
;; Theme
;; ========================================

;; Popular theme options (uncomment one you like):
;; - doom-themes: Modern, popular themes
;; - modus-themes: Built-in, excellent contrast
;; - ef-themes: Modern, accessible themes

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  ;; Load theme
  (load-theme 'doom-one t)  ; Options: doom-one, doom-dracula, doom-nord, etc.

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects org-mode's native fontification
  (doom-themes-org-config))

;; Alternative: Modus themes (built-in Emacs 28+)
;; (use-package modus-themes
;;   :straight nil  ; Built-in
;;   :config
;;   (setq modus-themes-bold-constructs t
;;         modus-themes-italic-constructs t)
;;   (load-theme 'modus-vivendi t))  ; or modus-operandi for light

;; ========================================
;; Which-Key - Command Discovery
;; ========================================

;; You already use which-key in IdeaVim, so this is familiar!
(use-package which-key
  :init
  (which-key-mode)
  :config
  ;; Popup delay (show after 0.3 seconds)
  (setq which-key-idle-delay 0.3)

  ;; Popup position (bottom, right, top, etc.)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-height 0.25)

  ;; Sort by key or by keymap description
  (setq which-key-sort-order 'which-key-key-order-alpha)

  ;; Allow C-h to trigger which-key manually
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-secondary-delay 0.05)

  ;; Maximum length for descriptions
  (setq which-key-max-description-length 50))

;; ========================================
;; Mode-Line - Doom Modeline
;; ========================================

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  ;; Size of icons (requires nerd-icons package)
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 4)

  ;; Show buffer encoding (utf-8, etc.)
  (setq doom-modeline-buffer-encoding nil)

  ;; Show file icon
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)

  ;; Show project name
  (setq doom-modeline-project-detection 'projectile)

  ;; Show vcs (git) status
  (setq doom-modeline-vcs-max-length 20)

  ;; Minor modes (hide to reduce clutter)
  (setq doom-modeline-minor-modes nil)

  ;; GitHub notifications (requires forge)
  (setq doom-modeline-github nil)

  ;; Display time
  (setq doom-modeline-time nil))

;; ========================================
;; Nerd Icons - Icon Support
;; ========================================

;; Required for doom-modeline and other packages
(use-package nerd-icons
  :config
  ;; First time setup: M-x nerd-icons-install-fonts
  )

;; Nerd icons for dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Nerd icons for completion (vertico/corfu)
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; ========================================
;; Dirvish - Modern Dired (Yazi-like)
;; ========================================

(use-package dirvish
  :config
  ;; Enable dirvish globally
  (dirvish-override-dired-mode)

  ;; Attributes to show
  (setq dirvish-attributes
        '(nerd-icons file-size collapse subtree-state vc-state git-msg))

  ;; Preview settings
  (setq dirvish-preview-dispatchers
        '(image video audio))

  ;; Cache directory
  (setq dirvish-cache-dir (expand-file-name "dirvish/" user-emacs-directory))

  ;; Side window width
  (setq dirvish-use-header-line t)
  (setq dirvish-mode-line-format
        '(:left (sort file-time " " file-size symlink) :right (omit yank index))))

;; ========================================
;; Rainbow Delimiters - Colored Parentheses
;; ========================================

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ========================================
;; Highlight Indent Guides
;; ========================================

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\│)
  (setq highlight-indent-guides-responsive 'top))

;; ========================================
;; Helpful - Better Help Buffers
;; ========================================

(use-package helpful
  :config
  ;; Replace default help commands
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command)

  ;; Also replace in leader keys
  (with-eval-after-load 'evil
    (with-eval-after-load 'general
      (when (fboundp 'tim/leader-keys)
        (tim/leader-keys
          "hf" '(helpful-callable :which-key "describe function")
          "hv" '(helpful-variable :which-key "describe variable")
          "hk" '(helpful-key :which-key "describe key")
          "hF" '(helpful-function :which-key "describe function")
          "hC" '(helpful-command :which-key "describe command"))))))

;; ========================================
;; Beacon - Highlight Cursor on Jump
;; ========================================

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-blink-when-window-scrolls nil)
  (setq beacon-blink-when-focused t))

;; ========================================
;; Pulsar - Pulse Line on Certain Actions
;; ========================================

(use-package pulsar
  :config
  (pulsar-global-mode 1)
  (setq pulsar-pulse-functions
        '(recenter-top-bottom
          move-to-window-line-top-bottom
          reposition-window
          forward-page
          backward-page
          scroll-up-command
          scroll-down-command
          evil-scroll-up
          evil-scroll-down
          evil-goto-first-line
          evil-goto-line))
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10))

;; ========================================
;; Fonts Configuration
;; ========================================

(defun tim/setup-fonts ()
  "Setup fonts for different faces."
  (condition-case nil
      (progn
        ;; Main font (monospace) - using Nerd Font version
        (set-face-attribute 'default nil
                            :font "JetBrainsMono Nerd Font"
                            :height 140)  ; 14pt

        ;; Fixed-pitch font (for code)
        (set-face-attribute 'fixed-pitch nil
                            :font "JetBrainsMono Nerd Font"
                            :height 140)

        ;; Variable-pitch font (for prose/org-mode)
        (set-face-attribute 'variable-pitch nil
                            :font "SF Pro"
                            :height 160))  ; Slightly larger for readability
    (error
     (message "Warning: Could not set fonts. Using defaults."))))

;; Apply fonts when GUI starts
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (tim/setup-fonts))))
  (tim/setup-fonts))

;; ========================================
;; Dashboard - Startup Screen
;; ========================================

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)

  ;; Dashboard items
  (setq dashboard-items '((recents . 10)
                          (projects . 10)
                          (bookmarks . 5)))

  ;; Icons
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  ;; Center content
  (setq dashboard-center-content t)

  ;; Banner
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-startup-banner 'logo)

  ;; Footer
  (setq dashboard-footer-messages
        '("Happy coding!"
          "\"The best time to plant a tree was 20 years ago. The second best time is now.\""
          "\"The only way to do great work is to love what you do.\""))

  ;; Set initial buffer
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))

;; ========================================
;; Window Dividers
;; ========================================

(use-package frame
  :straight nil  ; Built-in
  :config
  ;; Nice window dividers
  (setq window-divider-default-right-width 2)
  (setq window-divider-default-bottom-width 2)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1))

;; ========================================
;; Transparency (Optional)
;; ========================================

;; Uncomment to enable transparency
;; (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; Function to toggle transparency
(defun tim/toggle-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (or (not alpha) (= (cadr alpha) 100))
        (set-frame-parameter nil 'alpha '(95 . 95))
      (set-frame-parameter nil 'alpha '(100 . 100)))))

(with-eval-after-load 'evil
  (with-eval-after-load 'general
    (when (fboundp 'tim/leader-keys)
      (tim/leader-keys
        "tT" '(tim/toggle-transparency :which-key "transparency")))))

;; ========================================
;; Ligatures (Optional)
;; ========================================

;; Enable ligatures for certain fonts (JetBrains Mono, Fira Code, etc.)
(use-package ligature
  :config
  ;; Enable in programming modes
  (ligature-set-ligatures 'prog-mode
                          '("--" "---" "==" "===" "!=" "!==" "=!="
                            "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***"
                            ";;" "!!" "??" "???" "?:" "?." "?=" "<:" ":<" ":>"
                            ">:" "<>" "<<<" ">>>" "<<" ">>" "||" "-|" "_|_" "|-"
                            "|=" "||=" "##" "###" "####" "#{" "#[" "]#" "#(" "#?"
                            "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$" "$>" "<+>"
                            "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                            "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>"
                            "<==>" "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" ">--"
                            "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<->"
                            "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]"
                            "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                            "|||>" "<|||" "<|>" "..." ".." ".=" ".-" "..<" ".?"
                            "::" ":::" ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/"
                            "/=" "//=" "/==" "@_" "__"))
  (global-ligature-mode t))

;;; ui.el ends here

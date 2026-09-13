;;; themes/doom-everforest-light-theme.el --- Everforest light, contrast-tuned -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Vendored fork of https://github.com/Cardoso1994/doom-everforest-theme
;; (itself based on https://github.com/sainnhe/everforest).
;;
;; The stock light palette was the real eye-strain culprit: on the "hard"
;; cream background every syntax color sat between 2.2:1 and 3.3:1 WCAG
;; contrast (yellow was 2.2:1) and even fg was only 5.4:1.  This fork keeps
;; the everforest hues and the cream background but darkens everything:
;;   - fg to 7.5:1; syntax colors to >= 4.8:1 (most ~5:1)
;;   - comments/line numbers (base5/base6) >= 4.5:1
;; The variant/palette defcustoms from upstream are gone; this IS the variant.

(require 'doom-themes)

(defgroup doom-everforest-light-theme nil
  "Options for doom-everforest-light."
  :group 'doom-themes)

(defcustom doom-everforest-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-everforest-light-theme
  :type '(choice integer boolean))

(def-doom-theme doom-everforest-light
  "A light theme inspired by Everforest, retuned for contrast"
  ;; name        default   256       16
  ((bg         '("#fffbef" nil       nil            ))
   (bg-alt     '("#f2efdf" nil       nil            ))
   (base0      '("#f0eed9" "black"   "black"        ))
   (base1      '("#e9e8d2" "#1e1e1e" "brightblack"  ))
   (base2      '("#e1ddcb" "#2e2e2e" "brightblack"  ))
   (base3      '("#bec5b2" "#262626" "brightblack"  ))
   (base4      '("#edf0cd" "#3f3f3f" "brightblack"  )) ;; bg_visual
   (base5      '("#6c7864" "#525252" "brightblack"  )) ;; 4.5:1
   (base6      '("#5e6c5c" "#6b6b6b" "brightblack"  )) ;; 5.4:1
   (base7      '("#525f51" "#979797" "brightblack"  )) ;; 6.5:1
   (base8      '("#465345" "#dfdfdf" "white"        )) ;; 7.9:1
   (fg         '("#49545b" "#bfbfbf" "brightwhite"  )) ;; 7.5:1
   (fg-alt     '("#5c6a72" "#2d2d2d" "white"        ))

   (grey       base5)
   (red        '("#c0322f" "#ff6655" "red"          )) ;; 5.4:1
   (orange     '("#b25008" "#dd8844" "brightred"    )) ;; 5.0:1
   (green      '("#657401" "#99bb66" "green"        )) ;; 5.0:1
   (teal       '("#277a5b" "#44b9b1" "brightgreen"  )) ;; aqua, 5.1:1
   (yellow     '("#8e6600" "#ECBE7B" "yellow"       )) ;; 5.0:1
   (blue       '("#2d739a" "#51afef" "brightblue"   )) ;; 5.0:1
   (dark-blue  '("#4b7b75" "#2257A0" "blue"         )) ;; 4.6:1
   (magenta    '("#b02a86" "#c678dd" "brightmagenta")) ;; purple, 5.8:1
   (violet     '("#b02a86" "#a9a1e1" "magenta"      ))
   (cyan       '("#277a5b" "#46D9FF" "brightcyan"   )) ;; aqua
   (dark-cyan  '("#237e5c" "#5699AF" "cyan"         )) ;; 4.8:1

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       base5)
   (doc-comments   base7)
   (constants      magenta)
   (functions      cyan)
   (keywords       teal)
   (methods        cyan)
   (operators      blue)
   (type           orange)
   (strings        green)
   (variables      blue)
   (numbers        magenta)
   (region         base2)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-everforest-light-padded-modeline
      (if (integerp doom-everforest-light-padded-modeline)
          doom-everforest-light-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt fg-alt)
   (modeline-bg     `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base0)))
   (modeline-bg-l   `(,(doom-darken (car bg-alt) 0.03) ,@(cdr base0)))
   (modeline-bg-inactive   `(,(car bg-alt) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))

  ;; --- extra faces ------------------------
  ((evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face :foreground comments)
   (font-lock-doc-face :inherit 'font-lock-comment-face :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background highlight)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; fill column indicator
   (fill-column-indicator :foreground bg-alt :background bg-alt)

   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; cursor
   (cursor :foreground fg :background blue)

   ;; dired
   (diredfl-compressed-file-name :foreground yellow)
   (diredfl-dir-heading :foreground teal)
   (diredfl-dir-name :foreground blue)
   (diredfl-deletion :foreground red :background (doom-lighten red 0.55))
   (diredfl-deletion-file-name :foreground red :background (doom-lighten red 0.55))
   (diredfl-file-name :foreground fg)
   (dired-flagged :foreground red :background (doom-lighten red 0.55))
   (diredfl-symlink :foreground magenta)

   ;; eshell
   (+eshell-prompt-git-branch :foreground cyan)

   ;; evil
   (evil-ex-lazy-highlight :foreground fg :background (doom-lighten orange 0.5))
   (evil-snipe-first-match-face :foreground bg :background orange)

   ;; lsp
   (lsp-face-highlight-read    :background base2)
   (lsp-face-highlight-textual :background base2)
   (lsp-face-highlight-write   :background base2)

   ;; magit
   (magit-section-heading :foreground blue :weight 'bold)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background bg-alt)

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)
   (org-drawer :foreground yellow)
   (org-document-info :foreground blue :weight 'bold)
   (org-document-info-keyword :foreground blue)
   (org-document-title :foreground blue)
   (org-block-begin-line :foreground dark-cyan :background (doom-blend bg teal 0.85))
   (org-block-end-line :foreground dark-cyan :background (doom-blend bg teal 0.85))
   (org-block :foreground fg :background (doom-blend bg teal 0.85))
   (org-meta-line :foreground cyan)
   (org-level-1 :foreground magenta :weight 'semi-bold :height 1.4)
   (org-level-2 :foreground cyan :weight 'semi-bold :height 1.2)
   (org-level-3 :foreground green :weight 'semi-bold :height 1.1)
   (org-level-4 :foreground yellow :weight 'semi-bold)
   (org-level-5 :foreground violet :weight 'semi-bold)
   (org-level-6 :foreground dark-cyan :weight 'semi-bold)
   (org-level-7 :foreground (doom-darken green 0.15) :weight 'semi-bold)
   (org-level-8 :foreground (doom-darken yellow 0.15) :weight 'semi-bold)

   ;; org-roam
   (org-roam-title :foreground orange :weight 'semi-bold)

   ;; rainbow delimiters
   (rainbow-delimiters-depth-1-face :foreground orange)
   (rainbow-delimiters-depth-2-face :foreground violet)
   (rainbow-delimiters-depth-3-face :foreground dark-cyan)
   (rainbow-delimiters-depth-4-face :foreground (doom-darken yellow 0.15))
   (show-paren-match :foreground bg :background (doom-darken red 0.15))

   ;; vertico
   (vertico-current :foreground fg :background base2)

   ;; others
   (isearch :foreground fg :background (doom-lighten magenta 0.6))
   (company-tooltip-common-selection :foreground bg-alt :background dark-blue))

  ;; --- extra variables ---------------------
  ())

;;; doom-everforest-light-theme.el ends here

;;; themes/doom-everforest-theme.el --- Everforest, contrast-tuned -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Vendored fork of https://github.com/Cardoso1994/doom-everforest-theme
;; (itself based on https://github.com/sainnhe/everforest).
;;
;; The stock palette caused eye fatigue: on the "hard" background, half the
;; syntax colors sat at 5-6.5:1 WCAG contrast.  This fork keeps the everforest
;; hues but retunes for legibility:
;;   - background dropped to #14191c (below everforest's bg_dim); this is
;;     where most of the contrast comes from — the foregrounds keep their
;;     everforest hues instead of being washed toward pastel
;;   - every syntax color >= 7.8:1 on that bg (most 8-10:1)
;;   - comments/line numbers (base5/base6) >= 5.5:1
;; The variant/palette defcustoms from upstream are gone; this IS the variant.

(require 'doom-themes)

(defgroup doom-everforest-theme nil
  "Options for doom-everforest."
  :group 'doom-themes)

(defcustom doom-everforest-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-everforest-theme
  :type '(choice integer boolean))

(def-doom-theme doom-everforest
  "A dark theme inspired by Everforest, retuned for contrast"
  ;; name        default   256       16
  ((bg         '("#14191c" nil       nil            ))
   (bg-alt     '("#0e1214" nil       nil            ))
   (base0      '("#1e2528" "black"   "black"        ))
   (base1      '("#2b343a" "#1e1e1e" "brightblack"  ))
   (base2      '("#3a454a" "#2e2e2e" "brightblack"  ))
   (base3      '("#445055" "#262626" "brightblack"  ))
   (base4      '("#503946" "#3f3f3f" "brightblack"  )) ;; bg_visual
   (base5      '("#858f83" "#525252" "brightblack"  )) ;; 5.5:1
   (base6      '("#89968d" "#6b6b6b" "brightblack"  )) ;; 5.9:1
   (base7      '("#9da9a0" "#979797" "brightblack"  )) ;; 7.5:1
   (base8      '("#b9c5bb" "#dfdfdf" "white"        )) ;; 10.2:1
   (fg         '("#e0d8c3" "#bfbfbf" "brightwhite"  )) ;; 12.9:1
   (fg-alt     '("#b9c0ab" "#2d2d2d" "white"        ))

   (grey       base6)
   (red        '("#eb9899" "#ff6655" "red"          )) ;; 8.2:1
   (orange     '("#e79d7b" "#dd8844" "brightred"    )) ;; 8.2:1
   (green      '("#a7c080" "#99bb66" "green"        )) ;; 9.1:1
   (teal       '("#83c092" "#44b9b1" "brightgreen"  )) ;; aqua, 8.6:1
   (yellow     '("#dbbc7f" "#ECBE7B" "yellow"       )) ;; 9.9:1
   (blue       '("#7fbbb3" "#51afef" "brightblue"   )) ;; 8.4:1
   (dark-blue  '("#71a39d" "#2257A0" "blue"         )) ;; 6.4:1
   (magenta    '("#d89eb9" "#c678dd" "brightmagenta")) ;; purple, 8.2:1
   (violet     '("#d89eb9" "#a9a1e1" "magenta"      ))
   (cyan       '("#83c092" "#46D9FF" "brightcyan"   )) ;; aqua
   (dark-cyan  '("#74ab82" "#5699AF" "cyan"         )) ;; 6.8:1

   ;; face categories -- required for all themes
   (highlight      cyan)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       base6)
   (doc-comments   base7)
   (constants      violet)
   (functions      cyan)
   (keywords       (doom-lighten teal 0.1))
   (methods        cyan)
   (operators      blue)
   (type           orange)
   (strings        green)
   (variables      blue)
   (numbers        magenta)
   (region         base1)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-everforest-padded-modeline
      (if (integerp doom-everforest-padded-modeline)
          doom-everforest-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt fg-alt)
   (modeline-bg     `(,(doom-lighten (car bg) 0.05) ,@(cdr base0)))
   (modeline-bg-l   `(,(doom-lighten (car bg) 0.08) ,@(cdr base0)))
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
   (diredfl-deletion :foreground red :background base4)
   (diredfl-deletion-file-name :foreground red :background base4)
   (diredfl-file-name :foreground fg)
   (dired-flagged :foreground red :background base4)
   (diredfl-symlink :foreground magenta)

   ;; eshell
   (+eshell-prompt-git-branch :foreground cyan)

   ;; evil
   (evil-ex-lazy-highlight :foreground fg :background (doom-darken orange 0.3))
   (evil-snipe-first-match-face :foreground bg :background orange)

   ;; lsp
   (lsp-face-highlight-read    :background base2)
   (lsp-face-highlight-textual :background base2)
   (lsp-face-highlight-write   :background base2)

   ;; magit
   (magit-section-heading :foreground blue :weight 'bold)

   ;; markdown-mode
   (markdown-markup-face :foreground base6)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background base0)

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)
   (org-drawer :foreground (doom-darken yellow 0.15))
   (org-document-info :foreground blue)
   (org-document-info-keyword :foreground dark-blue)
   (org-document-title :foreground blue)
   (org-block-begin-line :foreground dark-cyan :background bg-alt)
   (org-block-end-line :foreground dark-cyan :background bg-alt)
   (org-block :foreground fg :background bg-alt)
   (org-meta-line :foreground dark-cyan)
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
   (vertico-current :foreground fg :background base1)

   ;; others
   (isearch :foreground bg :background violet)
   (company-tooltip-common-selection :foreground bg-alt :background dark-blue))

  ;; --- extra variables ---------------------
  ())

;;; doom-everforest-theme.el ends here

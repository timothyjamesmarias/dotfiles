;;; doom-islands-dark-theme.el --- JetBrains Islands Dark -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Ported from the editor scheme and UI theme bundled with IntelliJ IDEA 2026.2:
;;   themes/islands/IslandSchemeDark.xml      (editor colors)
;;   themes/islands/ManyIslandsDark.theme.json (UI chrome colors)
;; inside lib/intellij.platform.ide.impl.jar. Every hex below is taken from
;; those files; the comment on each line names the JetBrains key it came from.
;;
;; The scheme colors only keywords, strings, numbers, constants, comments and
;; function declarations. Identifiers, types, parameters, properties, operators
;; and brackets are all plain text, and that sparseness is the point.
;;
;;; Code:

(require 'doom-themes)

(defgroup doom-islands-dark-theme nil
  "Options for the `doom-islands-dark' theme."
  :group 'doom-themes)

(defcustom doom-islands-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-islands-dark-theme
  :type '(choice integer boolean))

(def-doom-theme doom-islands-dark
  "JetBrains Islands Dark, ported from the bundled IntelliJ scheme."

  ;; name        default   256       16
  ((bg         '("#191a1c" nil       nil            )) ; TEXT background / gray-10
   (bg-alt     '("#212326" nil       nil            )) ; gray-20, inline editor bg
   (base0      '("#191a1c" "black"   "black"        ))
   (base1      '("#1f2024" "#1e1e1e" "brightblack"  )) ; CARET_ROW_COLOR
   (base2      '("#26282c" "#2e2e2e" "brightblack"  )) ; gray-30, popups / status bar
   (base3      '("#2b2d30" "#262626" "brightblack"  )) ; INFORMATION_HINT, diff separators
   (base4      '("#33353b" "#3f3f3f" "brightblack"  )) ; gray-40, borders
   (base5      '("#4b5059" "#525252" "brightblack"  )) ; LINE_NUMBERS_COLOR
   (base6      '("#6f737a" "#6b6b6b" "brightblack"  )) ; WHITESPACES, disabled text
   (base7      '("#7a7e85" "#979797" "brightblack"  )) ; DEFAULT_LINE_COMMENT
   (base8      '("#a1a3ab" "#dfdfdf" "white"        )) ; LINE_NUMBER_ON_CARET_ROW_COLOR
   (fg         '("#bcbec4" "#bfbfbf" "brightwhite"  )) ; TEXT foreground
   (fg-alt     '("#ced0d6" "#2d2d2d" "white"        )) ; CARET_COLOR, DOC_CODE

   (grey       base7)
   (red        '("#f75464" "#ff6655" "red"          )) ; WRONG_REFERENCES
   (orange     '("#cf8e6d" "#dd8844" "brightred"    )) ; DEFAULT_KEYWORD
   (green      '("#6aab73" "#99bb66" "green"        )) ; DEFAULT_STRING
   (teal       '("#2aacb8" "#44b9b1" "brightgreen"  )) ; DEFAULT_NUMBER
   (yellow     '("#f2c55c" "#ECBE7B" "yellow"       )) ; WARNING_ATTRIBUTES
   (blue       '("#56a8f5" "#51afef" "brightblue"   )) ; DEFAULT_FUNCTION_DECLARATION
   (dark-blue  '("#548af7" "#2257A0" "blue"         )) ; HYPERLINK_ATTRIBUTES
   (magenta    '("#c77dbb" "#c678dd" "brightmagenta")) ; DEFAULT_CONSTANT
   (violet     '("#b189f5" "#a9a1e1" "magenta"      )) ; FOLLOWED_HYPERLINK
   (cyan       '("#16baac" "#46D9FF" "brightcyan"   )) ; TYPE_PARAMETER_NAME
   (dark-cyan  '("#2fbaa3" "#5699AF" "cyan"         )) ; HTML_CUSTOM_TAG_NAME

   ;; face categories -- required for all themes
   (highlight      dark-blue)
   (vertical-bar   base2)
   (selection      "#2a4371")                       ; selection-bg-active / blue-50
   (builtin        orange)
   (comments       base7)
   (doc-comments   "#5f826b")                       ; DEFAULT_DOC_COMMENT
   (constants      magenta)
   (functions      blue)
   (keywords       orange)
   (methods        "#57aaf7")                       ; DEFAULT_INSTANCE_METHOD
   (operators      fg)
   (type           fg)                              ; DEFAULT_CLASS_REFERENCE
   (strings        green)
   (variables      fg)
   (numbers        teal)
   (region         "#2a4371")
   (error          red)
   (warning        yellow)
   (success        "#73bd79")                       ; FILESTATUS_ADDED
   (vc-modified    "#375fad")                       ; MODIFIED_LINES_COLOR
   (vc-added       "#549159")                       ; ADDED_LINES_COLOR
   (vc-deleted     "#868a91")                       ; DELETED_LINES_COLOR

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-islands-dark-padded-modeline
      (if (integerp doom-islands-dark-padded-modeline)
          doom-islands-dark-padded-modeline 4)))

   (modeline-fg          fg)
   (modeline-fg-alt      base7)
   (modeline-bg          base2)                     ; StatusBar.background / gray-30
   (modeline-bg-inactive bg-alt))

  ;; --- extra faces ------------------------
  (((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground base8)
   (hl-line :background base1)
   (cursor :background fg-alt)
   (fringe :background bg)

   ;; Islands keeps line and block comments upright; only doc comments are italic.
   (font-lock-comment-face :foreground comments)
   (font-lock-doc-face :foreground doc-comments :slant 'italic)
   (font-lock-doc-markup-face :foreground "#68a67e")        ; DEFAULT_DOC_MARKUP
   ;; Everything below is plain text in the scheme.
   (font-lock-type-face :foreground fg)
   (font-lock-variable-name-face :foreground fg)
   (font-lock-variable-use-face :foreground fg)
   (font-lock-property-name-face :foreground fg)
   (font-lock-property-use-face :foreground fg)
   (font-lock-function-call-face :foreground fg)
   (font-lock-operator-face :foreground fg)
   (font-lock-bracket-face :foreground fg)
   (font-lock-delimiter-face :foreground fg)
   (font-lock-punctuation-face :foreground fg)
   (font-lock-escape-face :foreground orange)               ; DEFAULT_VALID_STRING_ESCAPE
   (font-lock-regexp-face :foreground "#42c3d4")            ; JS.REGEXP
   (font-lock-warning-face :foreground red)

   (show-paren-match :background "#43454a" :weight 'bold)    ; MATCHED_BRACE_ATTRIBUTES
   (show-paren-mismatch :foreground red :weight 'bold)
   (isearch :background "#2d543f" :foreground fg-alt)       ; SEARCH_RESULT_ATTRIBUTES
   (lazy-highlight :background "#114957" :foreground fg)    ; TEXT_SEARCH_RESULT_ATTRIBUTES
   (match :background "#114957" :foreground fg)
   (evil-ex-lazy-highlight :inherit 'lazy-highlight)
   (highlight-indent-guides-character-face :foreground "#323438") ; INDENT_GUIDE
   (fill-column-indicator :foreground "#323438")            ; RIGHT_MARGIN_COLOR
   (whitespace-space :foreground base6)
   (whitespace-tab :foreground base6)
   (trailing-whitespace :background "#402929")              ; ERROR_HINT
   (link :foreground dark-blue :underline t)
   (link-visited :foreground violet :underline t)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)
   (solaire-mode-line-face :inherit 'mode-line)
   (solaire-mode-line-inactive-face :inherit 'mode-line-inactive)

   (vertico-current :background base2)
   (corfu-default :background base2)
   (corfu-current :background selection)
   (completions-common-part :foreground "#538af9" :weight 'bold) ; CompletionPopup.matchForeground
   (orderless-match-face-0 :foreground "#538af9" :weight 'bold)

   (diff-hl-change :foreground vc-modified :background vc-modified)
   (diff-hl-insert :foreground vc-added :background vc-added)
   (diff-hl-delete :foreground vc-deleted :background vc-deleted)
   (lsp-face-highlight-read :background "#373b39")          ; IDENTIFIER_UNDER_CARET
   (lsp-face-highlight-write :background "#402f33")         ; WRITE_IDENTIFIER_UNDER_CARET
   (eglot-highlight-symbol-face :background "#373b39")

   ;; Project tree. Islands draws tree text in text-default (#d1d3d9), a step
   ;; brighter than editor text, with gray icons and VCS status by file color.
   (treemacs-root-face :foreground "#d1d3d9" :weight 'bold :height 1.0)
   (treemacs-directory-face :foreground "#d1d3d9")
   (treemacs-file-face :foreground "#d1d3d9")
   (treemacs-tags-face :foreground base8)
   (treemacs-git-modified-face :foreground "#70aeff")      ; FILESTATUS_MODIFIED
   (treemacs-git-added-face :foreground "#73bd79")         ; FILESTATUS_ADDED
   (treemacs-git-untracked-face :foreground "#e88f89")     ; FILESTATUS_UNKNOWN
   (treemacs-git-ignored-face :foreground "#d69a6b")       ; FILESTATUS_IGNORED
   (treemacs-git-conflict-face :foreground "#de6a66")      ; FILESTATUS_MERGED_WITH_CONFLICTS
   (treemacs-git-renamed-face :foreground "#70aeff")       ; FILESTATUS_RENAMED
   (doom-themes-treemacs-root-face :foreground "#d1d3d9" :weight 'bold)
   (doom-themes-treemacs-file-face :foreground "#8b8e94" :slant 'normal) ; gray-90 icon stroke
   (treemacs-nerd-icons-root-face :foreground "#d1d3d9" :weight 'bold :height 1.0)
   (treemacs-nerd-icons-file-face :foreground "#8b8e94")

   (org-level-1 :foreground blue)
   (org-level-2 :foreground magenta)
   (org-level-3 :foreground green)
   (org-level-4 :foreground orange)
   (org-block :background bg-alt :extend t)
   (org-block-begin-line :background bg-alt :foreground comments :extend t)
   (org-block-end-line :background bg-alt :foreground comments :extend t)
   (markdown-code-face :background bg-alt :extend t)))

;;; doom-islands-dark-theme.el ends here

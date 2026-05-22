;;; modules/dwim-nav.el --- DWIM navigation dispatcher -*- lexical-binding: t; -*-
;;
;; Tiered, context-aware navigation system for `gd`.
;; Dispatch order: context rules → LSP (with def/ref flip) → xref/tags → grep.
;;
;; Rules are registered via `+dwim-nav-rule!' and filtered by major mode,
;; detected framework, and a predicate function.  Shared matchers (e.g.,
;; CSS class extraction) live here; framework-specific resolvers stay in
;; their own modules.

;;; Rule data structure & registry

(cl-defstruct (+dwim-nav-rule (:constructor +dwim-nav-rule--create))
  "A context-aware navigation rule."
  id          ; symbol — unique identifier
  modes       ; list of major-mode symbols, or '(:all)
  frameworks  ; list of framework symbols, or nil for any
  predicate   ; (lambda () ...) → non-nil when this rule applies
  handler     ; (lambda (identifier) ...) → result protocol value
  priority    ; integer — lower = tried first (default 50)
  label)      ; string for echo-area tier indicator

(defvar +dwim-nav-rules nil
  "List of `+dwim-nav-rule' structs, kept sorted by priority.")

;;; Registration macro

(defmacro +dwim-nav-rule! (id &rest props)
  "Register a DWIM navigation rule.

ID is a unique symbol.  PROPS is a plist:
  :modes      — list of major modes, or (:all)
  :frameworks — list of framework symbols (from `+tim/detect-framework'), or nil
  :predicate  — function returning non-nil when rule applies
  :handler    — function taking IDENTIFIER, returning result protocol value
  :priority   — integer (default 50; lower = higher priority)
  :label      — display string for tier indicator"
  (declare (indent defun))
  `(progn
     (setq +dwim-nav-rules
           (cl-delete ',id +dwim-nav-rules :key #'+dwim-nav-rule-id))
     (push (+dwim-nav-rule--create
            :id ',id
            :modes ',(plist-get props :modes)
            :frameworks ',(plist-get props :frameworks)
            :predicate ,(plist-get props :predicate)
            :handler ,(plist-get props :handler)
            :priority ,(or (plist-get props :priority) 50)
            :label ,(or (plist-get props :label) (symbol-name id)))
           +dwim-nav-rules)
     (setq +dwim-nav-rules
           (cl-sort +dwim-nav-rules #'< :key #'+dwim-nav-rule-priority))))

;;; Result protocol
;;
;; Handlers return one of:
;;   nil                          — no result, fall through
;;   '(jumped)                    — already navigated to target
;;   (candidates . LIST-OF-PLISTS) — multiple matches for completing-read
;;
;; Each candidate plist: (:label STRING :file PATH :line N)

(defun +dwim-nav-result-jumped ()
  "Signal that the handler already jumped to the result."
  '(jumped))

(defun +dwim-nav-result-candidates (items)
  "Return a candidates result.
ITEMS is a list of plists with :label :file :line."
  (cons 'candidates items))

;;; Shared matchers

(defun +dwim-nav-css-class-at-point ()
  "Extract CSS class name at point in a template buffer, or nil.
Works in slim, erb, haml, html, and web-mode buffers.
Reusable across frameworks — pair with a framework-specific resolver."
  (save-excursion
    (let ((line (thing-at-point 'line t)))
      (when line
        (cond
         ;; Slim: .class-name or div.class-name
         ((and (derived-mode-p 'slim-mode)
               (string-match "\\.\\([a-zA-Z_-][a-zA-Z0-9_-]*\\)" line))
          (let ((classes (let (result (start 0))
                           (while (string-match
                                   "\\.\\([a-zA-Z_-][a-zA-Z0-9_-]*\\)" line start)
                             (push (match-string 1 line) result)
                             (setq start (match-end 0)))
                           (nreverse result))))
            (if (= (length classes) 1)
                (car classes)
              (completing-read "Class: " classes nil t))))
         ;; ERB/HTML: class="foo bar baz"
         ((string-match "class=\"\\([^\"]+\\)\"" line)
          (let ((classes (split-string (match-string 1 line) " " t)))
            (if (= (length classes) 1)
                (car classes)
              (completing-read "Class: " classes nil t))))
         ;; Ruby hash-style: class: "foo bar" or class: 'foo bar'
         ((string-match "class:\\s-*[\"']\\([^\"']+\\)[\"']" line)
          (let ((classes (split-string (match-string 1 line) " " t)))
            (if (= (length classes) 1)
                (car classes)
              (completing-read "Class: " classes nil t))))
         ;; Fallback: symbol at point if it looks like a class name
         (t
          (let ((sym (thing-at-point 'symbol t)))
            (when (and sym (string-match-p "\\`[a-zA-Z_-][a-zA-Z0-9_-]*\\'" sym))
              sym))))))))

;;; Internal helpers

(defun +dwim-nav--applicable-rules (framework)
  "Return rules applicable to current buffer's mode and FRAMEWORK."
  (cl-remove-if-not
   (lambda (rule)
     (and
      ;; Mode check
      (or (equal (+dwim-nav-rule-modes rule) '(:all))
          (cl-some (lambda (m) (derived-mode-p m))
                   (+dwim-nav-rule-modes rule)))
      ;; Framework check (nil means any)
      (or (null (+dwim-nav-rule-frameworks rule))
          (memq framework (+dwim-nav-rule-frameworks rule)))
      ;; Predicate
      (or (null (+dwim-nav-rule-predicate rule))
          (ignore-errors (funcall (+dwim-nav-rule-predicate rule))))))
   +dwim-nav-rules))

(defun +dwim-nav--try-rules (rules identifier)
  "Try RULES in priority order.  Returns (LABEL . result) or nil.
Same-priority rules that both produce results get candidates merged."
  (let ((by-priority (seq-group-by #'+dwim-nav-rule-priority rules)))
    (cl-loop
     for (_pri . group) in (cl-sort by-priority #'< :key #'car)
     do (let ((results nil))
          (dolist (rule group)
            (when-let ((r (condition-case err
                              (funcall (+dwim-nav-rule-handler rule) identifier)
                            (error
                             (message "dwim-nav rule %s error: %s"
                                      (+dwim-nav-rule-id rule) err)
                             nil))))
              (push (cons (+dwim-nav-rule-label rule) r) results)))
          (when results
            (cl-return
             (if (= (length results) 1)
                 ;; Single rule matched
                 (let ((label (caar results))
                       (r (cdar results)))
                   (cons (format "[%s]" label) r))
               ;; Multiple same-priority rules: merge candidates
               (+dwim-nav--merge-results results))))))))

(defun +dwim-nav--merge-results (results)
  "Merge multiple rule RESULTS into a single candidates result.
RESULTS is a list of (LABEL . result) conses."
  (let ((all-candidates nil))
    (dolist (entry results)
      (let ((label (car entry))
            (r (cdr entry)))
        (pcase r
          ('(jumped)
           ;; A rule already jumped — honour it, label and done
           (cl-return-from +dwim-nav--merge-results
             (cons (format "[%s]" label) r)))
          (`(candidates . ,items)
           (dolist (item items)
             (push (plist-put (copy-sequence item)
                              :tier-label label)
                   all-candidates))))))
    (when all-candidates
      (cons "[multi]"
            (cons 'candidates (nreverse all-candidates))))))

(defun +dwim-nav--xref-at-point-p (xref-item origin)
  "Return non-nil if XREF-ITEM points to the same location as ORIGIN marker."
  (condition-case nil
      (let* ((loc (xref-item-location xref-item))
             (marker (xref-location-marker loc)))
        (and (eq (marker-buffer marker) (marker-buffer origin))
             (<= (abs (- (marker-position marker) (marker-position origin)))
                 (length (or (thing-at-point 'symbol) "")))))
    (error nil)))

(defun +dwim-nav--try-lsp (identifier origin)
  "Try eglot definition.  If already on definition, flip to references.
Returns result protocol value or nil."
  (when (and (eglot-server-capable :definitionProvider)
             identifier)
    (condition-case nil
        (let ((defs (xref-backend-definitions 'eglot identifier)))
          (cond
           ((null defs) nil)
           ;; Single def at current position → flip to references
           ((and (= (length defs) 1)
                 (+dwim-nav--xref-at-point-p (car defs) origin))
            (if (eglot-server-capable :referencesProvider)
                (let ((refs (xref-backend-references 'eglot identifier)))
                  (when refs
                    (if (= (length refs) 1)
                        (progn
                          (xref-pop-to-location (xref-item-location (car refs)))
                          '(jumped))
                      (xref--show-xrefs (lambda () refs) nil)
                      '(jumped))))
              nil))
           ;; Single def elsewhere → jump
           ((= (length defs) 1)
            (xref-pop-to-location (xref-item-location (car defs)))
            '(jumped))
           ;; Multiple defs → show picker
           (t
            (xref--show-defs (lambda () defs) nil)
            '(jumped))))
      (error nil))))

(defun +dwim-nav--try-xref (identifier)
  "Try non-eglot xref backends for IDENTIFIER.
Returns result protocol value or nil."
  (when identifier
    (condition-case nil
        ;; Get xref backends, skipping eglot if present
        (let* ((xref-backend-functions
                (cl-remove-if (lambda (fn)
                                (eq (ignore-errors (funcall fn)) 'eglot))
                              xref-backend-functions))
               (backend (xref-find-backend)))
          (when backend
            (let ((defs (xref-backend-definitions backend identifier)))
              (cond
               ((null defs) nil)
               ((= (length defs) 1)
                (xref-pop-to-location (xref-item-location (car defs)))
                '(jumped))
               (t
                (xref--show-defs (lambda () defs) nil)
                '(jumped))))))
      (error nil))))

(defun +dwim-nav--try-grep (identifier)
  "Fallback: ripgrep search for IDENTIFIER in project.
Returns result protocol value or nil."
  (when (and identifier (doom-project-p))
    (condition-case nil
        (progn
          (+default/search-project-for-symbol-at-point
           identifier (doom-project-root))
          '(jumped))
      (error nil))))

(defun +dwim-nav--jump-to (item)
  "Jump to ITEM, a plist with :file and :line."
  (find-file (plist-get item :file))
  (goto-char (point-min))
  (forward-line (1- (plist-get item :line))))

(defun +dwim-nav--handle-result (result tier-label origin)
  "Handle a dispatch RESULT, showing TIER-LABEL in the echo area.
ORIGIN is the marker where the jump started."
  (pcase result
    ('(jumped)
     (message "%s" (propertize tier-label 'face 'success)))
    (`(candidates . ,items)
     (let* ((choices
             (mapcar (lambda (it)
                       (let ((label (or (plist-get it :tier-label)
                                        tier-label)))
                         (cons (format "%s %s"
                                       (propertize (format "[%s]" label)
                                                   'face 'warning)
                                       (plist-get it :label))
                               it)))
                     items))
            (pick (completing-read "Jump to: " (mapcar #'car choices) nil t))
            (item (cdr (assoc pick choices))))
       (when item
         (+dwim-nav--jump-to item)
         (message "%s" (propertize tier-label 'face 'success))))))
  ;; Record jump if we moved
  (when (and origin
             (not (and (eq (current-buffer) (marker-buffer origin))
                       (= (point) (marker-position origin)))))
    (with-current-buffer (marker-buffer origin)
      (better-jumper-set-jump (marker-position origin)))))

;;; Main dispatch

;;;###autoload
(defun +dwim/navigate (identifier &optional arg)
  "DWIM navigation: context rules → LSP (with def/ref flip) → tags → grep.

With prefix ARG, fall through to Doom's `+lookup/definition' handler picker."
  (interactive (list (doom-thing-at-point-or-region) current-prefix-arg))
  (cond
   ;; Prefix arg → escape hatch to Doom's handler picker
   (arg
    (+lookup/definition identifier arg))
   ;; Nothing under point
   ((null identifier)
    (user-error "Nothing under point"))
   ;; Normal DWIM dispatch
   (t
    (let* ((origin (point-marker))
           (framework (ignore-errors
                        (+tim/detect-framework (doom-project-root))))
           (result nil)
           (tier-label nil))

      ;; Tier 1: Context rules
      (let ((applicable (+dwim-nav--applicable-rules framework)))
        (when applicable
          (when-let ((r (+dwim-nav--try-rules applicable identifier)))
            (setq tier-label (car r)
                  result (cdr r)))))

      ;; Tier 2: LSP (eglot) with definition↔reference flip
      (unless result
        (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
          (let ((r (+dwim-nav--try-lsp identifier origin)))
            (when r
              (setq result r
                    tier-label "[LSP]")))))

      ;; Tier 3: xref/tags (non-eglot backends)
      (unless result
        (let ((r (+dwim-nav--try-xref identifier)))
          (when r
            (setq result r
                  tier-label "[tags]"))))

      ;; Tier 4: grep fallback
      (unless result
        (let ((r (+dwim-nav--try-grep identifier)))
          (when r
            (setq result r
                  tier-label "[rg]"))))

      ;; Handle result or report failure
      (if result
          (+dwim-nav--handle-result result tier-label origin)
        (set-marker origin nil)
        (user-error "No navigation target found for %S"
                    (substring-no-properties identifier)))

      (set-marker origin nil)))))

;;; Keybinding

(map! :nv "gd" #'+dwim/navigate)

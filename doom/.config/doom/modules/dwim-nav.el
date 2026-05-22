;;; modules/dwim-nav.el --- DWIM navigation dispatcher -*- lexical-binding: t; -*-
;;
;; Context-aware navigation layer for `gd`.
;;
;; Registers a Doom lookup handler at the front of
;; `+lookup-definition-functions' that runs context-aware rules
;; (filtered by major mode, framework, and predicate) before Doom's
;; default handlers (eglot/xref, dumb-jump, project search).
;;
;; If no context rule matches, Doom's chain runs unmodified.
;; A thin `gd' wrapper adds def→ref flip: when the entire definition
;; chain fails (returns nil), try references instead.
;;
;; Rules are registered by framework modules (rails-nav.el, maizzle.el,
;; etc.) via `+dwim-nav-rule!'.  Shared matchers (e.g., CSS class
;; extraction) live here for reuse across frameworks.

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
                 (let ((label (caar results))
                       (r (cdar results)))
                   (cons (format "[%s]" label) r))
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

(defun +dwim-nav--jump-to (item)
  "Jump to ITEM, a plist with :file and :line."
  (find-file (plist-get item :file))
  (goto-char (point-min))
  (forward-line (1- (plist-get item :line))))

(defun +dwim-nav--show-candidates (items tier-label)
  "Display ITEMS via completing-read with TIER-LABEL prefix."
  (let* ((choices (mapcar (lambda (it)
                            (cons (format "%s %s"
                                         tier-label
                                         (plist-get it :label))
                                  it))
                          items))
         (pick (completing-read "Jump to: " (mapcar #'car choices) nil t))
         (item (cdr (assoc pick choices))))
    (when item
      (+dwim-nav--jump-to item))))

;;; Doom lookup handler

(defun +dwim-nav-lookup-handler (identifier)
  "Doom lookup handler: tries context-aware rules before LSP/dumb-jump.
Added to `+lookup-definition-functions' at the front of the chain.

Returns non-nil if a context rule handled the navigation, nil to
let Doom's default handlers (xref/eglot, dumb-jump, grep) run."
  (let* ((framework (ignore-errors
                      (+tim/detect-framework (doom-project-root))))
         (applicable (+dwim-nav--applicable-rules framework)))
    (when applicable
      (let ((result (+dwim-nav--try-rules applicable identifier)))
        (when result
          (let ((tier-label (car result))
                (r (cdr result)))
            (pcase r
              ('(jumped)
               (message "%s" tier-label)
               t)
              (`(candidates . ,items)
               (+dwim-nav--show-candidates items tier-label)
               t))))))))

;; Prepend to Doom's lookup chain (depth -90 = before all defaults)
(add-hook '+lookup-definition-functions #'+dwim-nav-lookup-handler -90)

;;; DWIM gd wrapper (def→ref flip)

(defun +dwim/navigate (identifier &optional arg)
  "DWIM go-to-definition with def→ref flip.

Runs `+lookup/definition' (which includes our context rules at the
front, then eglot/xref, dumb-jump, and grep).

If the cursor doesn't move (eglot resolved to current position, i.e.,
we're already on the definition) and eglot is active, tries
`+lookup/references' instead.  Also catches the case where the entire
chain fails (`user-error')."
  (interactive (list (doom-thing-at-point-or-region) current-prefix-arg))
  (let ((origin-buf (current-buffer))
        (origin-pos (point)))
    (condition-case nil
        (let ((result (+lookup/definition identifier arg)))
          ;; If deferred (consult picker open), don't interfere.
          ;; If cursor didn't move and eglot is active, flip to references.
          (when (and (not (eq result 'deferred))
                     (not arg)
                     (eq (current-buffer) origin-buf)
                     (= (point) origin-pos)
                     identifier
                     (fboundp 'eglot-managed-p)
                     (eglot-managed-p))
            (+lookup/references identifier)))
      (user-error
       (if (and (not arg)
                identifier
                (fboundp 'eglot-managed-p)
                (eglot-managed-p))
           (+lookup/references identifier)
         (user-error "Couldn't find the definition of %S"
                     (substring-no-properties identifier)))))))

(map! :nv "gd" #'+dwim/navigate)
(after! eglot
  (map! :map eglot-mode-map :nv "gd" #'+dwim/navigate))

;;; Suppress built-in etags prompts in modes without LSP.
;; Emacs registers etags--xref-backend globally.  In buffers without
;; eglot, it becomes the only xref backend and prompts for a TAGS file.
;; Our dwim-nav rules and dumb-jump handle navigation instead.
(defun +dwim-nav--disable-etags-backend-h ()
  "Remove the built-in etags xref backend from this buffer."
  (setq-local xref-backend-functions
              (remove #'etags--xref-backend xref-backend-functions)))

(add-hook 'ruby-mode-hook #'+dwim-nav--disable-etags-backend-h)
(add-hook 'ruby-ts-mode-hook #'+dwim-nav--disable-etags-backend-h)

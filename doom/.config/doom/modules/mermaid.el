;;; modules/mermaid.el --- Mermaid editing and live preview -*- lexical-binding: t; -*-

;; Static preview: shells out to mmdc, renders an SVG image (works anywhere).
;; Live preview: xwidget-webkit buffer running mermaid.js client-side, with
;; debounced re-render on edit (needs a GUI frame in an xwidgets build).

(use-package! ob-mermaid
  :defer t
  :config
  (setq ob-mermaid-cli-path (executable-find "mmdc")
        ob-mermaid-default-config-file
        (expand-file-name "mermaid-config.json" doom-user-dir)))

(after! org
  (add-hook 'org-babel-after-execute-hook
            (defun +mermaid-redisplay-images-h ()
              (when (org-in-src-block-p)
                (org-redisplay-inline-images)))))

(use-package! mermaid-mode
  :defer t
  :mode "\\.mmd\\'")

;; --- Source extraction ---

(defun +mermaid--extract-fenced-block ()
  "Extract mermaid source from a markdown fenced block at point."
  (save-excursion
    (let ((pos (point)))
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward "^```mermaid\\s-*$" nil t)
          (let ((start (line-beginning-position 2)))
            (when (re-search-forward "^```\\s-*$" nil t)
              (let ((end (line-beginning-position)))
                (when (and (<= start pos) (<= pos (line-end-position)))
                  (throw 'found (buffer-substring-no-properties start end)))))))))))

(defun +mermaid--extract-org-block ()
  "Extract mermaid source from an org src block at point."
  (when (and (derived-mode-p 'org-mode) (org-in-src-block-p))
    (let ((info (org-babel-get-src-block-info 'light)))
      (when (string= (car info) "mermaid")
        (nth 1 info)))))

(defun +mermaid--source-at-point ()
  "Return the mermaid source at point, or nil.
Checks markdown fenced blocks, org src blocks, active region, then
the whole buffer for .mmd files or `mermaid-mode' buffers."
  (or (+mermaid--extract-fenced-block)
      (+mermaid--extract-org-block)
      (when (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end)))
      (when (or (derived-mode-p 'mermaid-mode)
                (string-suffix-p ".mmd" (or (buffer-file-name) "")))
        (buffer-substring-no-properties (point-min) (point-max)))))

;; --- Static preview (mmdc fallback) ---

(defun +mermaid/preview ()
  "Render the mermaid diagram at point in a side buffer.
Works from markdown fenced blocks, org src blocks, .mmd files, or region."
  (interactive)
  (let* ((src (+mermaid--source-at-point))
         (mmdc (or (executable-find "mmdc")
                   (user-error "mmdc not found — install @mermaid-js/mermaid-cli")))
         (infile (make-temp-file "mermaid-" nil ".mmd"))
         (outfile (concat (file-name-sans-extension infile) ".svg"))
         (config (expand-file-name "mermaid-config.json" doom-user-dir)))
    (unless src (user-error "No mermaid diagram found at point"))
    (with-temp-file infile (insert src))
    (let ((exit (call-process mmdc nil "*mermaid-errors*" nil
                              "-i" infile "-o" outfile
                              "-c" config)))
      (unless (and (zerop exit) (file-exists-p outfile))
        (pop-to-buffer "*mermaid-errors*")
        (user-error "mmdc failed (exit %d)" exit)))
    (let ((buf (get-buffer-create "*mermaid-preview*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-image (create-image outfile 'svg nil
                                      :max-width (/ (frame-pixel-width) 2)))
          (goto-char (point-min))
          (special-mode)))
      (display-buffer buf '(display-buffer-in-side-window
                            (side . right) (window-width . 0.45))))))

;; --- Live preview assets ---

(defconst +mermaid-live--asset-dir
  (expand-file-name "mermaid-live/" doom-cache-dir)
  "Directory holding the npm-installed mermaid.js and generated HTML.")

(defconst +mermaid-live--template
  (expand-file-name "assets/mermaid-live/preview.html.in" doom-user-dir)
  "HTML template with a <!--MERMAID_JS--> placeholder.")

(defconst +mermaid-live--buffer-name "*mermaid-live*")

(defvar +mermaid-live-debounce 0.4
  "Idle seconds after an edit before the live preview re-renders.")

(defun +mermaid-live--js-file ()
  (expand-file-name "node_modules/mermaid/dist/mermaid.min.js"
                    +mermaid-live--asset-dir))

(defun +mermaid-live--html-file ()
  (expand-file-name "preview.html" +mermaid-live--asset-dir))

(defun +mermaid--live-ensure-assets ()
  "Install mermaid.js and assemble preview.html if needed.
Returns the path to the generated HTML file."
  (let ((js (+mermaid-live--js-file))
        (html (+mermaid-live--html-file)))
    (unless (file-exists-p js)
      (unless (y-or-n-p "mermaid.js not installed; run npm install now? ")
        (user-error "Live preview needs mermaid.js"))
      (make-directory +mermaid-live--asset-dir t)
      (let ((npm (or (executable-find "npm") (user-error "npm not found")))
            (default-directory +mermaid-live--asset-dir))
        (message "Installing mermaid into %s..." +mermaid-live--asset-dir)
        (unless (zerop (call-process npm nil "*mermaid-live-install*" nil
                                     "install" "mermaid"
                                     "--prefix" +mermaid-live--asset-dir))
          (pop-to-buffer "*mermaid-live-install*")
          (user-error "npm install mermaid failed"))
        (unless (file-exists-p js)
          (user-error "npm install succeeded but %s is missing" js))))
    (when (or (not (file-exists-p html))
              (file-newer-than-file-p +mermaid-live--template html)
              (file-newer-than-file-p js html))
      (with-temp-file html
        (insert-file-contents +mermaid-live--template)
        (goto-char (point-min))
        (unless (search-forward "<!--MERMAID_JS-->" nil t)
          (user-error "Placeholder missing in %s" +mermaid-live--template))
        (replace-match "" t t)
        (insert-file-contents js)))
    html))

;; --- Live preview session ---

(defvar +mermaid-live--source-buffer nil)
(defvar +mermaid-live--timer nil)
(defvar +mermaid-live--xwidget nil)
(defvar +mermaid-live--ready nil)

(defun +mermaid--live-theme-args ()
  "Return (MERMAID-THEME . BG-HEX) matching the current Emacs theme."
  (let* ((bg (face-background 'default))
         (dark (color-dark-p (color-name-to-rgb bg))))
    (cons (if dark "dark" "default") bg)))

(defun +mermaid--live-exec (script)
  (when (and +mermaid-live--xwidget (xwidget-live-p +mermaid-live--xwidget))
    (xwidget-webkit-execute-script +mermaid-live--xwidget script)))

(defun +mermaid--live-render ()
  "Push the current mermaid source into the webview."
  (when (and +mermaid-live--ready
             (buffer-live-p +mermaid-live--source-buffer))
    (when-let* ((src (with-current-buffer +mermaid-live--source-buffer
                       (+mermaid--source-at-point)))
                (b64 (base64-encode-string (encode-coding-string src 'utf-8) t)))
      (+mermaid--live-exec (format "renderMermaid('%s');" b64)))))

(defun +mermaid--live-schedule-render-h (&rest _)
  (when +mermaid-live--timer
    (cancel-timer +mermaid-live--timer))
  (setq +mermaid-live--timer
        (run-with-idle-timer +mermaid-live-debounce nil #'+mermaid--live-render)))

(defun +mermaid--live-poll-ready (attempts)
  "Poll the webview until mermaid.js is loaded, then do the first render.
The page signals readiness via `document.title' because
`xwidget-webkit-execute-script' callbacks don't fire on macOS NS builds."
  (cond
   ((not (and +mermaid-live--xwidget (xwidget-live-p +mermaid-live--xwidget))))
   ((<= attempts 0)
    (message "mermaid-live: webview never became ready")
    (+mermaid--live-teardown))
   ((equal (xwidget-webkit-title +mermaid-live--xwidget) "mermaid-ready")
    (setq +mermaid-live--ready t)
    (pcase-let ((`(,theme . ,bg) (+mermaid--live-theme-args)))
      (+mermaid--live-exec (format "initTheme('%s', '%s');" theme bg)))
    (+mermaid--live-render))
   (t (run-at-time 0.1 nil #'+mermaid--live-poll-ready (1- attempts)))))

(defun +mermaid--live-resize-h (&rest _)
  (when-let* ((buf (get-buffer +mermaid-live--buffer-name))
              (win (get-buffer-window buf))
              ((and +mermaid-live--xwidget (xwidget-live-p +mermaid-live--xwidget))))
    (xwidget-resize +mermaid-live--xwidget
                    (window-body-width win t)
                    (window-body-height win t))))

(defun +mermaid--live-create-webview ()
  "Create the preview buffer with a fresh xwidget session and load the page."
  (let ((html (+mermaid--live-ensure-assets))
        (buf (get-buffer-create +mermaid-live--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode))
      (local-set-key "q" #'+mermaid--live-teardown)
      (when (fboundp 'evil-local-set-key)
        (evil-local-set-key 'normal "q" #'+mermaid--live-teardown)))
    (let ((win (display-buffer buf '(display-buffer-in-side-window
                                     (side . right) (window-width . 0.45)))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-min))
            (insert " ")  ; xwidget-insert needs a char to carry the display prop
            (setq +mermaid-live--xwidget
                  (xwidget-insert (point-min) 'webkit +mermaid-live--buffer-name
                                  (window-body-width win t)
                                  (window-body-height win t))))))
      (xwidget-webkit-goto-uri +mermaid-live--xwidget
                               (concat "file://" html)))))

(defun +mermaid--live-cleanup-h ()
  (+mermaid--live-teardown))

(defun +mermaid--live-teardown ()
  "Tear down the live preview session.  Idempotent."
  (interactive)
  (when +mermaid-live--timer
    (cancel-timer +mermaid-live--timer)
    (setq +mermaid-live--timer nil))
  (when (buffer-live-p +mermaid-live--source-buffer)
    (with-current-buffer +mermaid-live--source-buffer
      (remove-hook 'after-change-functions #'+mermaid--live-schedule-render-h t)
      (remove-hook 'kill-buffer-hook #'+mermaid--live-cleanup-h t)))
  (remove-hook 'window-size-change-functions #'+mermaid--live-resize-h)
  (setq +mermaid-live--source-buffer nil
        +mermaid-live--xwidget nil
        +mermaid-live--ready nil)
  (when-let* ((buf (get-buffer +mermaid-live--buffer-name)))
    (with-current-buffer buf
      (remove-hook 'kill-buffer-hook #'+mermaid--live-cleanup-h t))
    ;; xwidget-kill-buffer-query-function would prompt (and block a
    ;; daemon eval); killing the preview is always intentional here.
    (let ((kill-buffer-query-functions
           (remq 'xwidget-kill-buffer-query-function
                 kill-buffer-query-functions)))
      (kill-buffer buf))))

(defun +mermaid/live-preview ()
  "Toggle a live mermaid preview of the diagram at point.
Renders client-side with mermaid.js in an embedded webview and
re-renders as you edit.  Supports pan (drag), zoom (wheel), and
reset (double-click)."
  (interactive)
  (if (eq (current-buffer) +mermaid-live--source-buffer)
      (+mermaid--live-teardown)
    (unless (featurep 'xwidget-internal)
      (user-error "This Emacs build lacks xwidget support"))
    (require 'xwidget)
    (unless (display-graphic-p)
      (user-error "Live preview needs a GUI frame"))
    (unless (+mermaid--source-at-point)
      (user-error "No mermaid diagram found at point"))
    (+mermaid--live-teardown)
    (setq +mermaid-live--source-buffer (current-buffer))
    (add-hook 'after-change-functions #'+mermaid--live-schedule-render-h nil t)
    (add-hook 'kill-buffer-hook #'+mermaid--live-cleanup-h nil t)
    (add-hook 'window-size-change-functions #'+mermaid--live-resize-h)
    (+mermaid--live-create-webview)
    (with-current-buffer +mermaid-live--buffer-name
      (add-hook 'kill-buffer-hook #'+mermaid--live-cleanup-h nil t))
    (+mermaid--live-poll-ready 50)))

;; --- Keybindings ---

(map! :leader
      (:prefix ("M" . "mermaid")
       :desc "Preview diagram (static)" "p" #'+mermaid/preview
       :desc "Live preview"             "l" #'+mermaid/live-preview))

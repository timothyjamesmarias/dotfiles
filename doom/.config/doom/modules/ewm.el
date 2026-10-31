;;; modules/ewm.el --- EWM (Emacs Wayland Manager) glue -*- lexical-binding: t; -*-

;; EWM runs Emacs as --fg-daemon and creates frames with `make-frame', never
;; through emacsclient. Doom defers theme, font and UI setup to
;; `server-after-make-frame-hook', which only emacsclient fires, so run it on
;; the first graphical frame ourselves. From the EWM wiki, "Doom Emacs" page.
(when (daemonp)
  (add-hook 'after-make-frame-functions
            (defun +ewm-trigger-server-hooks-h (frame)
              (when (display-graphic-p frame)
                (remove-hook 'after-make-frame-functions #'+ewm-trigger-server-hooks-h)
                (with-selected-frame frame
                  (run-hooks 'server-after-make-frame-hook))))))

;; pgtk draws child frames inside the parent frame's Wayland surface, and EWM
;; paints application views on top of that surface. Vertico's popup therefore
;; vanishes behind any app window it overlaps, so use the plain minibuffer.
(remove-hook 'vertico-mode-hook #'vertico-posframe-mode)
;; The minibuffer pushes app content up when it grows. Keep it to about a
;; quarter of a 54-line frame instead of Doom's 17 lines.
(setq vertico-count 12)

;;; Keys: macOS semantics
;;
;; Super is the application shortcut modifier, as Cmd is on the Mac, and Ctrl
;; is left for text editing. The compositor rewrites these chords only while
;; an app has focus; Emacs buffers see the originals. Per-app exceptions go
;; in `ewm-update-app-hook'. Compositor commands that collide with an app
;; shortcut move to Shift+Super, EWM's existing frame layer. Emacs reports a
;; shifted letter as the uppercase letter, so those bind as "s-F", not "S-s-f".
;; Frames stay on s-1..9; Firefox selects tabs with Alt+digit on Linux.

(defconst +ewm-editing-keys
  '(("C-n" . "<down>") ("C-p" . "<up>")
    ("C-f" . "<right>") ("C-b" . "<left>")
    ("C-a" . "<home>") ("C-e" . "<end>")
    ("C-d" . "<delete>") ("C-k" . "S-<end> <delete>") ("C-w" . "C-<backspace>")
    ("M-f" . "C-<right>") ("M-b" . "C-<left>")
    ("M-d" . "C-<delete>") ("M-<backspace>" . "C-<backspace>"))
  "Cocoa-style editing keys sent to application text fields.")

(defconst +ewm-cmd-keys
  '("a" "x" "z" "S-z" "f" "g" "S-g" "t" "S-t" "w" "n" "l" "r" "s" "p" "S-p"
    "o" "q" "h" "j" "y" "=" "-" "0" "[" "]")
  "Keys that apps receive with Ctrl when typed with Super, as Cmd on the Mac.
Copy and paste are already EWM defaults.")

;; Doom treats *name* buffers as unreal and keeps them out of workspaces, whose
;; filter `next-buffer' and friends consult. App windows are real buffers.
(add-hook 'doom-real-buffer-functions #'ewm-surface-buffer-p)

;;; Launcher: a Spotlight-style list of running and launchable apps. The same
;;; sources also join `consult-buffer', since each app window is a buffer and
;;; killing it closes the app.

(defvar +ewm-consult-source-running-apps
  `(:name "Running apps" :narrow ?r :category buffer
    :items ,(lambda () (mapcar #'buffer-name (seq-filter #'ewm-surface-buffer-p (buffer-list))))
    :state ,#'consult--buffer-state)
  "`consult-buffer' source for open application windows.")

(defun +ewm-launchable-apps ()
  "Installed desktop apps. Emacs is the desktop here, so a nested one is never wanted."
  (seq-remove (lambda (name) (string-prefix-p "Emacs" name))
              (mapcar #'car (ewm-list-xdg-apps))))

(defvar +ewm-consult-source-emacs
  `(:name "Emacs" :narrow ?e :category app
    :items ("Emacs")
    :action ,(lambda (_name) (switch-to-buffer (+ewm--last-emacs-buffer))))
  "`consult-buffer' source so \"Emacs\" means back to the last buffer, not a new process.")

(defvar +ewm-consult-source-xdg-apps
  `(:name "Apps" :narrow ?a :category app
    :items ,#'+ewm-launchable-apps
    :action ,#'ewm-launch-xdg-command)
  "`consult-buffer' source for installed desktop applications.")

(after! consult
  (add-to-list 'consult-buffer-sources '+ewm-consult-source-running-apps 'append)
  (add-to-list 'consult-buffer-sources '+ewm-consult-source-xdg-apps 'append))

(defun +ewm/launcher ()
  "Switch to a running app or launch an installed one."
  (interactive)
  (require 'consult)
  (consult--multi '(+ewm-consult-source-running-apps +ewm-consult-source-emacs +ewm-consult-source-xdg-apps)
                  :prompt "App: "))

(defun +ewm--app-ring ()
  "App windows in creation order, so the cycle is stable."
  (seq-sort-by #'ewm-surface-buffer-p #'< (seq-filter #'ewm-surface-buffer-p (buffer-list))))

(defun +ewm--last-emacs-buffer ()
  "Most recently used real buffer that is not an app window, else the dashboard."
  (or (seq-find (lambda (buffer) (and (doom-real-buffer-p buffer) (not (ewm-surface-buffer-p buffer))))
                (buffer-list))
      (doom-fallback-buffer)))

(defun +ewm/next-app ()
  "Cycle app windows like Cmd-Tab, with Emacs as one stop in the ring."
  (interactive)
  (let* ((ring (+ewm--app-ring))
         (pos (seq-position ring (current-buffer)))
         (target (cond ((null pos) (car ring))
                       ((nth (1+ pos) ring))
                       (t (+ewm--last-emacs-buffer)))))
    (when target (switch-to-buffer target))))

(after! ewm
  (setopt ewm-surface-emulate-keys
          (append +ewm-editing-keys
                  (mapcar (lambda (key) (cons (concat "s-" key) (concat "C-" key)))
                          +ewm-cmd-keys)
                  '((kill-ring-save . "C-c") (yank . "C-v"))))
  ;; Compositor commands whose Super chord is now an app shortcut.
  (dolist (key '("s-d" "s-f" "s-l" "s-t" "s-w"))
    (keymap-unset ewm-mode-map key t))
  (define-keymap :keymap ewm-mode-map
    "s-SPC" #'+ewm/launcher
    "s-<tab>" (ewm-key #'+ewm/next-app :repeat)
    "s-<return>" #'+vterm/toggle
    "s-b" (lambda () (interactive) (ewm-launch-app "firefox"))
    "s-F" #'ewm-toggle-fullscreen
    "s-L" #'ewm-lock-session
    "s-N" #'ewm-frame-new
    "s-W" #'ewm-frame-close))

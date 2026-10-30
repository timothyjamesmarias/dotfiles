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
;; shortcut move to Shift+Super, EWM's existing frame layer.

(defconst +ewm-editing-keys
  '(("C-n" . "<down>") ("C-p" . "<up>")
    ("C-f" . "<right>") ("C-b" . "<left>")
    ("C-a" . "<home>") ("C-e" . "<end>")
    ("C-d" . "<delete>") ("C-k" . "S-<end> <delete>")
    ("M-f" . "C-<right>") ("M-b" . "C-<left>")
    ("M-d" . "C-<delete>") ("M-<backspace>" . "C-<backspace>"))
  "Cocoa-style editing keys sent to application text fields.")

(defconst +ewm-cmd-keys
  '("a" "x" "z" "S-z" "f" "g" "S-g" "t" "S-t" "w" "n" "l" "r" "s" "p" "S-p"
    "o" "q" "h" "j" "y" "=" "-" "0" "[" "]" "1" "2" "3" "4" "5" "6" "7" "8" "9")
  "Keys that apps receive with Ctrl when typed with Super, as Cmd on the Mac.
Copy and paste are already EWM defaults.")

(after! ewm
  (setopt ewm-surface-emulate-keys
          (append +ewm-editing-keys
                  (mapcar (lambda (key) (cons (concat "s-" key) (concat "C-" key)))
                          +ewm-cmd-keys)
                  '((kill-ring-save . "C-c") (yank . "C-v"))))
  ;; Compositor commands whose Super chord is now an app shortcut.
  (dolist (key '("s-d" "s-f" "s-l" "s-t" "s-w"
                 "s-1" "s-2" "s-3" "s-4" "s-5" "s-6" "s-7" "s-8" "s-9"))
    (keymap-unset ewm-mode-map key t))
  (define-keymap :keymap ewm-mode-map
    "s-SPC" #'ewm-launch-app
    "s-<return>" #'+vterm/toggle
    "s-b" (lambda () (interactive) (ewm-launch-app "firefox"))
    "S-s-f" #'ewm-toggle-fullscreen
    "S-s-l" #'ewm-lock-session
    "S-s-n" #'ewm-frame-new
    "S-s-w" #'ewm-frame-close
    "S-s-1" #'ewm-frame-select "S-s-2" #'ewm-frame-select "S-s-3" #'ewm-frame-select
    "S-s-4" #'ewm-frame-select "S-s-5" #'ewm-frame-select "S-s-6" #'ewm-frame-select
    "S-s-7" #'ewm-frame-select "S-s-8" #'ewm-frame-select "S-s-9" #'ewm-frame-select))

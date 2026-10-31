;;; modules/ewm-launcher.el --- Raycast-style launcher for EWM -*- lexical-binding: t; -*-

;; One fuzzy entry point on s-SPC. Each kind of result is a consult source,
;; Embark on C-; acts on the selected result, and input that matches nothing
;; falls through to a calculator, a URL opener or a web search. M-RET submits
;; the raw input even when something matches.

(defconst +ewm-launcher-search-url "https://duckduckgo.com/?q=%s"
  "Web search for free text; %s becomes the URL-encoded query.")

(defconst +ewm-launcher-screenshot-directory "~/Pictures/Screenshots/"
  "Where the screenshot actions save their PNG files.")

(defconst +ewm-launcher-new-frame-timeout 2
  "Seconds to wait for a new frame's surface before launching into it.")

;; The minibuffer holds the keyboard until it closes, and the app's text field
;; only becomes active again once the compositor hands focus back.
(defconst +ewm-launcher-field-settle-delay 0.15
  "Seconds to wait after the launcher closes before typing into an app field.")

(defvar +ewm-launcher-history nil
  "Names picked in the launcher, newest first, used to rank results.")

(defun +ewm-launcher--by-recency (names)
  "NAMES with recently picked ones first, the rest in their original order."
  (seq-sort-by (lambda (name) (or (seq-position +ewm-launcher-history name) most-positive-fixnum))
               #'< names))

(defun +ewm-launcher--run (command &optional on-success)
  "Run COMMAND, an argument list, in the background.
Report a failure with the command's output; call ON-SUCCESS on exit 0."
  (let ((buffer (generate-new-buffer (format " *ewm-launcher %s*" (car command)))))
    (make-process
     :name (car command) :command command :buffer buffer :noquery t
     :sentinel (lambda (process _event)
                 (unless (process-live-p process)
                   (+ewm-launcher--finish process on-success))))))

(defun +ewm-launcher--finish (process on-success)
  "Report PROCESS's failure or call ON-SUCCESS, then drop its output buffer."
  (let ((output (with-current-buffer (process-buffer process) (string-trim (buffer-string)))))
    (kill-buffer (process-buffer process))
    (if (zerop (process-exit-status process))
        (when on-success (funcall on-success))
      (message "%s failed: %s" (process-name process) output))))

;;; Apps

(defvar +ewm-consult-source-running-apps
  `(:name "Running apps" :narrow ?r :category buffer
    :items ,(lambda () (mapcar #'buffer-name (seq-filter #'ewm-surface-buffer-p (buffer-list))))
    :state ,#'consult--buffer-state)
  "`consult-buffer' source for open application windows.")

(defun +ewm-launchable-apps ()
  "Installed desktop apps, without Emacs: it is the desktop, never a nested app."
  (seq-remove (lambda (name) (string-prefix-p "Emacs" name))
              (mapcar #'car (ewm-list-xdg-apps))))

(defvar +ewm-consult-source-emacs
  `(:name "Emacs" :narrow ?e :category app
    :items ("Emacs")
    :action ,(lambda (_name) (switch-to-buffer (+ewm--last-emacs-buffer))))
  "`consult-buffer' source where \"Emacs\" means the last buffer, not a new process.")

(defvar +ewm-launcher--desktop-entries nil
  "Desktop entries as (NAME FILE COMMENT), reread each time the app list is built.")

(defun +ewm-launcher--desktop-field (key)
  "The first KEY= value in the current desktop file buffer, or nil.
The main [Desktop Entry] group comes first, so its value wins over actions'."
  (goto-char (point-min))
  (when (re-search-forward (concat "^" key "=\\(.*\\)$") nil t)
    (match-string 1)))

(defun +ewm-launcher--read-desktop-entries ()
  "Every installed desktop file as (NAME FILE COMMENT)."
  (mapcar (lambda (file)
            (with-temp-buffer
              (insert-file-contents file)
              (list (+ewm-launcher--desktop-field "Name") file (+ewm-launcher--desktop-field "Comment"))))
          (mapcan (lambda (dir) (file-expand-wildcards (expand-file-name "applications/*.desktop" dir)))
                  (cons (xdg-data-home) (xdg-data-dirs)))))

(defun +ewm-launcher--app-items ()
  "Launchable app names ranked by recent use; refreshes the desktop entries."
  (setq +ewm-launcher--desktop-entries (+ewm-launcher--read-desktop-entries))
  (+ewm-launcher--by-recency (+ewm-launchable-apps)))

(defun +ewm-launcher--app-comment (name)
  "The Comment line of the app called NAME, for the annotation column."
  (when-let* ((comment (nth 2 (assoc name +ewm-launcher--desktop-entries))))
    (concat " " (propertize comment 'face 'completions-annotations))))

(defvar +ewm-consult-source-xdg-apps
  `(:name "Apps" :narrow ?a :category app :history +ewm-launcher-history
    :items ,#'+ewm-launcher--app-items
    :annotate ,#'+ewm-launcher--app-comment
    :action ,#'ewm-launch-xdg-command)
  "`consult-buffer' source for installed desktop applications.")

(after! consult
  (add-to-list 'consult-buffer-sources '+ewm-consult-source-running-apps 'append)
  (add-to-list 'consult-buffer-sources '+ewm-consult-source-xdg-apps 'append))

(defun +ewm-launcher--desktop-file (name)
  "The .desktop file whose Name is NAME, or nil."
  (nth 1 (assoc name (+ewm-launcher--read-desktop-entries))))

(defun +ewm-launcher-open-desktop-file (name)
  "Visit the .desktop file of the app called NAME."
  (find-file (or (+ewm-launcher--desktop-file name)
                 (user-error "No desktop file named %s" name))))

(defun +ewm-launcher-copy-command (name)
  "Copy the command line that launches the app called NAME."
  (kill-new (cdr (assoc name (ewm-list-xdg-apps))))
  (message "Copied the command for %s" name))

(defun +ewm-launcher-launch-in-new-frame (name)
  "Open a new frame and launch the app called NAME into it."
  ;; The new frame is selected only once it maps, so use the one returned.
  (let* ((frame (ewm-frame-new))
         (ewm-launch-hint (list :frame-surface-id (+ewm-launcher--await-surface frame))))
    (ewm-launch-xdg-command name)))

(defun +ewm-launcher--await-surface (frame)
  "FRAME's compositor surface id, waiting for a new frame to map."
  (let ((deadline (+ (float-time) +ewm-launcher-new-frame-timeout)))
    (while (and (not (frame-parameter frame 'ewm-surface-id)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (or (frame-parameter frame 'ewm-surface-id)
        (user-error "The new frame did not appear within %ss" +ewm-launcher-new-frame-timeout))))

(defvar-keymap +ewm-embark-app-map
  :doc "Embark actions on an installed app."
  "RET" #'ewm-launch-xdg-command
  "o" #'+ewm-launcher-launch-in-new-frame
  "d" #'+ewm-launcher-open-desktop-file
  "w" #'+ewm-launcher-copy-command)

(after! embark
  (set-keymap-parent +ewm-embark-app-map embark-general-map)
  (add-to-list 'embark-keymap-alist '(app . +ewm-embark-app-map)))

;;; Commands: each one opens its own view

(defconst +ewm-launcher-commands
  '(("Clipboard history" . +ewm/clipboard-history)
    ("Emoji" . +ewm/emoji)
    ("Find file in home" . +ewm/find-file-in-home)
    ("Recent files" . consult-recent-file)
    ("Bookmarks" . consult-bookmark)
    ("Wi-Fi networks" . +ewm/wifi-connect)
    ("Bluetooth devices" . +ewm/bluetooth-toggle-device)
    ("Processes" . proced)
    ("Calculator" . calc))
  "Launcher commands: the name shown and the interactive command it runs.")

(defun +ewm-launcher--first-doc-line (command)
  "COMMAND's first documentation line, for the annotation column."
  (when-let* ((doc (documentation command)))
    (concat " " (propertize (car (split-string doc "\n")) 'face 'completions-annotations))))

(defvar +ewm-launcher-source-commands
  `(:name "Commands" :narrow ?c :category ewm-launcher-command :history +ewm-launcher-history
    :items ,(lambda () (+ewm-launcher--by-recency (mapcar #'car +ewm-launcher-commands)))
    :annotate ,(lambda (name) (+ewm-launcher--first-doc-line (alist-get name +ewm-launcher-commands nil nil #'equal)))
    :action ,(lambda (name) (call-interactively (alist-get name +ewm-launcher-commands nil nil #'equal))))
  "Launcher source for commands that open their own view.")

;;; System actions

(defconst +ewm-launcher-system-actions
  '(("Lock screen" . ewm-lock-session)
    ("Suspend" . ("systemctl" "suspend"))
    ("Reboot" . ("systemctl" "reboot"))
    ("Power off" . ("systemctl" "poweroff"))
    ("Wi-Fi on" . ("nmcli" "radio" "wifi" "on"))
    ("Wi-Fi off" . ("nmcli" "radio" "wifi" "off"))
    ("Bluetooth on" . ("bluetoothctl" "power" "on"))
    ("Bluetooth off" . ("bluetoothctl" "power" "off"))
    ("Toggle microphone mute" . ewm-mic-mute)
    ("Turn off monitors" . ewm-power-off-monitors)
    ("Screenshot region" . +ewm/screenshot-region)
    ("Screenshot screen" . +ewm/screenshot-screen))
  "System actions: the name shown and a command symbol or an argument list to run.")

(defconst +ewm-launcher-confirmed-actions '("Reboot" "Power off")
  "System actions that ask before running, since they end the session.")

(defun +ewm-launcher-run-system-action (name)
  "Run the system action called NAME, confirming first when it ends the session."
  (let ((action (alist-get name +ewm-launcher-system-actions nil nil #'equal)))
    (when (and (member name +ewm-launcher-confirmed-actions)
               (not (yes-or-no-p (format "%s now? " name))))
      (user-error "Cancelled"))
    (if (symbolp action)
        (call-interactively action)
      (+ewm-launcher--run action))))

(defvar +ewm-launcher-source-system
  `(:name "System" :narrow ?s :category ewm-launcher-system :history +ewm-launcher-history
    :items ,(lambda () (+ewm-launcher--by-recency (mapcar #'car +ewm-launcher-system-actions)))
    :action ,#'+ewm-launcher-run-system-action)
  "Launcher source for session, radio and screenshot actions.")

(defun +ewm-launcher--screenshot-file ()
  "A fresh timestamped PNG path in the screenshot directory."
  (make-directory +ewm-launcher-screenshot-directory t)
  (expand-file-name (format-time-string "%Y-%m-%d_%H-%M-%S.png") +ewm-launcher-screenshot-directory))

(defun +ewm-launcher--report-screenshot (file)
  "Copy FILE's path and say where the screenshot went."
  (kill-new file)
  (message "Screenshot saved to %s (path copied)" file))

(defun +ewm/screenshot-screen ()
  "Save a screenshot of every output."
  (interactive)
  (let ((file (+ewm-launcher--screenshot-file)))
    (+ewm-launcher--run (list "grim" file) (lambda () (+ewm-launcher--report-screenshot file)))))

(defun +ewm/screenshot-region ()
  "Select a region with the pointer and save a screenshot of it."
  (interactive)
  (let ((file (+ewm-launcher--screenshot-file)))
    (make-process
     :name "slurp" :command '("slurp") :noquery t
     :filter (lambda (_process geometry)
               (+ewm-launcher--run (list "grim" "-g" (string-trim geometry) file)
                                   (lambda () (+ewm-launcher--report-screenshot file)))))))

;;; Free text: calculator, URL, web search

(defconst +ewm-launcher-calc-prefix "="
  "Input starting with this always goes to Calc.")

(defconst +ewm-launcher-arithmetic-regexp "\\`[-+*/^%(). 0-9]+\\'"
  "Input made only of numbers and operators, treated as a calculation.")

(defconst +ewm-launcher-url-regexp
  "\\`\\(https?://\\S-+\\|[[:alnum:]-]+\\(\\.[[:alnum:]-]+\\)*\\.[[:alpha:]]\\{2,\\}\\(/\\S-*\\)?\\)\\'"
  "Input that looks like a URL, with or without a scheme.")

(defun +ewm-launcher--calculation-p (text)
  "Non-nil when TEXT asks for a calculation."
  (or (string-prefix-p +ewm-launcher-calc-prefix text)
      (and (string-match-p +ewm-launcher-arithmetic-regexp text)
           (string-match-p "[0-9]" text)
           (string-match-p "[-+*/^%]" text))))

(defun +ewm-launcher--calculate (text)
  "Evaluate TEXT with Calc, show the result and copy it."
  (require 'calc)
  (let* ((expression (string-trim (string-remove-prefix +ewm-launcher-calc-prefix text)))
         (result (calc-eval expression)))
    (when (consp result)
      (user-error "Calc could not read %s: %s" expression (cadr result)))
    (kill-new result)
    (message "%s = %s (copied)" expression result)))

(defun +ewm-launcher--browse-url (text)
  "Open TEXT as a URL, adding https:// when it has no scheme."
  (browse-url (if (string-match-p "\\`https?://" text) text (concat "https://" text))))

(defun +ewm-launcher--search-web (text)
  "Search the web for TEXT."
  (browse-url (format +ewm-launcher-search-url (url-hexify-string text))))

(defun +ewm-launcher-handle-free-text (input)
  "Calculate, open or search INPUT, which matched no launcher result."
  (let ((text (string-trim input)))
    (cond ((string-empty-p text) nil)
          ((+ewm-launcher--calculation-p text) (+ewm-launcher--calculate text))
          ((string-match-p +ewm-launcher-url-regexp text) (+ewm-launcher--browse-url text))
          (t (+ewm-launcher--search-web text)))))

(defvar +ewm-launcher-source-free-text
  `(:name "Free text" :default t :hidden t
    :items ,(lambda () nil)
    :new ,#'+ewm-launcher-handle-free-text)
  "Launcher source that receives input matching no other result.")

;;; The launcher

(defun +ewm/launcher ()
  "Find an app, command or system action, or calculate, open or search the input."
  (interactive)
  (require 'consult)
  (consult--multi '(+ewm-consult-source-running-apps
                    +ewm-consult-source-emacs
                    +ewm-consult-source-xdg-apps
                    +ewm-launcher-source-commands
                    +ewm-launcher-source-system
                    +ewm-launcher-source-free-text)
                  :prompt "> "
                  :require-match nil
                  ;; Sources arrive ranked by recent use; keep that order.
                  :sort nil))

;;; Sub-views

(defun +ewm-launcher--deliver-text (text)
  "Make TEXT the clipboard, and type it into an active app text field."
  (setq kill-ring (delete text kill-ring))
  (kill-new text)
  (message "Copied %s" (truncate-string-to-width text 40 nil nil "…"))
  (run-with-timer +ewm-launcher-field-settle-delay nil #'+ewm-launcher--type-into-field text))

(defun +ewm-launcher--type-into-field (text)
  "Commit TEXT to the active app text field, if any."
  (when-let* ((field (ewm-text-input-surrounding)))
    (ewm-im-commit text (car field))))

(defun +ewm/clipboard-history ()
  "Pick an earlier clipboard entry and make it current again.
It is also typed into the active app text field, if any."
  (interactive)
  (+ewm-launcher--deliver-text (consult--read-from-kill-ring)))

(defun +ewm/emoji ()
  "Pick an emoji by name and copy it.
It is also typed into the active app text field, if any."
  (interactive)
  (require 'emoji)
  ;; `emoji-search' only inserts into the current buffer; its reader is the
  ;; part that is useful when the target is an app.
  (emoji--init)
  (+ewm-launcher--deliver-text (car (emoji--read-emoji))))

(defun +ewm/find-file-in-home ()
  "Find a file anywhere under the home directory with fd."
  (interactive)
  (consult-fd "~/"))

(defun +ewm-launcher--wifi-networks ()
  "Visible networks as (SSID SIGNAL SECURITY), strongest first, one per SSID."
  (seq-uniq
   (seq-keep (lambda (line)
               (when (string-match "\\`\\([0-9]+\\):\\([^:]*\\):\\(.+\\)\\'" line)
                 (list (match-string 3 line) (match-string 1 line) (match-string 2 line))))
             (process-lines "nmcli" "-t" "-e" "no" "-f" "SIGNAL,SECURITY,SSID" "device" "wifi" "list"))
   (lambda (a b) (equal (car a) (car b)))))

(defun +ewm-launcher--wifi-connect-command (network)
  "The nmcli call that joins NETWORK, a (SSID SIGNAL SECURITY) list."
  (pcase-let ((`(,ssid ,_ ,security) network))
    (cond ((member ssid (process-lines "nmcli" "-t" "-f" "NAME" "connection" "show"))
           (list "nmcli" "connection" "up" "id" ssid))
          ((string-empty-p security)
           (list "nmcli" "device" "wifi" "connect" ssid))
          ;; nmcli only takes a new network's password as an argument, so it
          ;; is briefly visible to local users in the process list.
          (t (list "nmcli" "device" "wifi" "connect" ssid
                   "password" (read-passwd (format "Password for %s: " ssid)))))))

(defun +ewm/wifi-connect ()
  "Join a visible Wi-Fi network, asking for a password only when it is new."
  (interactive)
  (let* ((networks (+ewm-launcher--wifi-networks))
         (completion-extra-properties
          (list :annotation-function
                (lambda (ssid)
                  (pcase-let ((`(,_ ,signal ,security) (assoc ssid networks)))
                    (format "  %s%%  %s" signal security)))))
         (network (assoc (completing-read "Wi-Fi: " networks nil t) networks)))
    (+ewm-launcher--run (+ewm-launcher--wifi-connect-command network)
                        (lambda () (message "Connected to %s" (car network))))))

(defun +ewm-launcher--bluetooth-devices (filter)
  "Devices bluetoothctl lists under FILTER, as (NAME . ADDRESS)."
  (seq-keep (lambda (line)
              (when (string-match "\\`Device \\(\\S-+\\) \\(.+\\)\\'" line)
                (cons (match-string 2 line) (match-string 1 line))))
            (process-lines "bluetoothctl" "devices" filter)))

(defun +ewm/bluetooth-toggle-device ()
  "Connect a paired Bluetooth device, or disconnect it when it is connected."
  (interactive)
  (let* ((paired (+ewm-launcher--bluetooth-devices "Paired"))
         (connected (mapcar #'cdr (+ewm-launcher--bluetooth-devices "Connected")))
         (completion-extra-properties
          (list :annotation-function
                (lambda (name) (if (member (cdr (assoc name paired)) connected) "  connected" ""))))
         (name (completing-read "Bluetooth: " paired nil t))
         (address (cdr (assoc name paired)))
         (verb (if (member address connected) "disconnect" "connect")))
    (+ewm-launcher--run (list "bluetoothctl" verb address)
                        (lambda () (message "Bluetooth: %s %sed" name verb)))))

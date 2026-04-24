;;; modules/docker.el --- Docker utilities -*- lexical-binding: t; -*-

(defun +tim/docker-stop-all ()
  "Stop all running Docker containers."
  (interactive)
  (let ((ids (string-trim (shell-command-to-string "docker ps -q"))))
    (if (string-empty-p ids)
        (message "No running containers")
      (shell-command (concat "docker stop " (string-join (split-string ids "\n") " ")))
      (message "Stopped all containers"))))

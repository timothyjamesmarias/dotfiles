;;; modules/sessionizer.el --- tmux-style session picker -*- lexical-binding: t; -*-

;; One fuzzy menu over live workspaces and known projects, like a tmux
;; sessionizer: picking a workspace switches to it; picking a project
;; opens it in its own workspace (created on the fly, same path as SPC p p).

(defun +tim/sessionizer ()
  "Fuzzily switch to a workspace, or open a known project as one."
  (interactive)
  (let* ((workspaces (+workspace-list-names))
         ;; Project roots already claimed by a live workspace (workspace names
         ;; don't always match `doom-project-name', e.g. after a project rename).
         (ws-roots (delq nil (mapcar (lambda (ws)
                                       (persp-parameter '+workspace-project
                                                        (persp-get-by-name ws)))
                                     workspaces)))
         (projects (cl-remove-if
                    (lambda (p)
                      (or (member (doom-project-name p) workspaces)
                          (cl-some (lambda (root)
                                     (ignore-errors (file-equal-p root p)))
                                   ws-roots)))
                    (projectile-relevant-known-projects)))
         (candidates
          (append
           (mapcar (lambda (ws) (cons ws (cons 'workspace ws))) workspaces)
           (mapcar (lambda (p) (cons (abbreviate-file-name p) (cons 'project p)))
                   projects)))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (category . session)
                        (group-function
                         . ,(lambda (cand transform)
                              (if transform cand
                                (if (eq (cadr (assoc cand candidates)) 'workspace)
                                    "Workspaces" "Projects")))))
                    (complete-with-action action candidates str pred))))
         (choice (completing-read "Session: " table nil t))
         (entry (cdr (assoc choice candidates))))
    (pcase entry
      (`(workspace . ,name) (+workspace/switch-to name))
      (`(project . ,path) (projectile-switch-project-by-name path)))))

(map! :leader
      :desc "Sessionizer" "TAB TAB" #'+tim/sessionizer)

;;; modules/buffers.el --- Project×workspace buffer cycling -*- lexical-binding: t; -*-

(defun +tim/project-workspace-buffers ()
  "Buffers that belong to both the current project and current workspace.
Falls back gracefully when one or both scopes are unavailable."
  (let* ((in-project (and (fboundp 'projectile-project-p)
                          (projectile-project-p)))
         (proj (and in-project (projectile-project-buffers)))
         (persp (and (bound-and-true-p persp-mode)
                     (fboundp 'persp-buffer-list)
                     (persp-buffer-list))))
    (cond
     ((and proj persp) (seq-intersection proj persp))
     (proj proj)
     (persp persp)
     (t (buffer-list)))))

(defun +tim/cycle-buffer (direction)
  "Cycle to the next (1) or previous (-1) project×workspace buffer."
  (let* ((bufs (+tim/project-workspace-buffers))
         (bufs (seq-filter
                (lambda (b)
                  (and (not (string-prefix-p " " (buffer-name b)))
                       (not (with-current-buffer b
                              (derived-mode-p 'vterm-mode
                                              'term-mode
                                              'eshell-mode
                                              'shell-mode
                                              'comint-mode
                                              'magit-mode)))
                       (if (fboundp 'doom-real-buffer-p)
                           (doom-real-buffer-p b)
                         t)))
                bufs))
         (bufs (sort bufs (lambda (a b)
                            (string< (buffer-name a) (buffer-name b))))))
    (if (or (null bufs) (<= (length bufs) 1))
        (if (> direction 0) (next-buffer) (previous-buffer))
      (let* ((cur (current-buffer))
             (idx (or (seq-position bufs cur) 0))
             (nxt (mod (+ idx direction) (length bufs))))
        (switch-to-buffer (nth nxt bufs))))))

(defun +tim/next-project-workspace-buffer ()
  "Cycle to the next buffer in the current project ∩ workspace."
  (interactive) (+tim/cycle-buffer 1))

(defun +tim/previous-project-workspace-buffer ()
  "Cycle to the previous buffer in the current project ∩ workspace."
  (interactive) (+tim/cycle-buffer -1))

(map! :n "C-n" #'+tim/next-project-workspace-buffer
      :n "C-p" #'+tim/previous-project-workspace-buffer)

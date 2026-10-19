;;; perspectives.lisp - Project-based perspective management for Lem
;;; Provides isolated buffer lists per project (like Emacs perspective-mode)

(defpackage :lem-perspectives
  (:use :cl :lem)
  (:export :perspective
           :*perspectives*
           :*current-perspective*
           :current-perspective
           :perspective-switch
           :perspective-new
           :perspective-kill
           :perspective-rename
           :perspective-list
           :perspective-buffers
           :find-perspective
           :perspective-new-command
           :perspective-kill-command
           :perspective-rename-command
           :perspective-list-command))

(in-package :lem-perspectives)

;;; Data Structures

(defstruct perspective
  "A perspective represents an isolated workspace with its own buffer list."
  name              ; String: user-visible name
  root              ; Pathname: project root directory (can be nil)
  buffers           ; List: buffers belonging to this perspective
  frame-id          ; Integer: associated frame ID (can be nil)
  created-at        ; Universal time
  last-accessed)    ; Universal time

;;; Global State

(defvar *perspectives* (make-hash-table :test 'equal)
  "Hash table mapping perspective names to perspective objects.")

(defvar *current-perspective* nil
  "The currently active perspective.")

(defvar *default-perspective-name* "main"
  "Name of the default perspective.")

;;; Utilities

(defun perspective-exists-p (name)
  "Check if a perspective with NAME exists."
  (gethash name *perspectives*))

(defun find-perspective (name)
  "Find perspective by NAME. Returns NIL if not found."
  (gethash name *perspectives*))

(defun all-perspectives ()
  "Return a list of all perspective objects."
  (loop for persp being the hash-values of *perspectives*
        collect persp))

(defun all-perspective-names ()
  "Return a list of all perspective names."
  (loop for name being the hash-keys of *perspectives*
        collect name))

(defun current-perspective ()
  "Get the current perspective, creating default if none exists."
  (or *current-perspective*
      (perspective-new *default-perspective-name*)))

;;; Perspective Management

(defun perspective-new (name &key root)
  "Create a new perspective with NAME and optional ROOT directory."
  (when (perspective-exists-p name)
    (editor-error "Perspective '~A' already exists" name))

  (let ((persp (make-perspective
                :name name
                :root (when root (pathname root))
                :buffers '()
                :frame-id nil
                :created-at (get-universal-time)
                :last-accessed (get-universal-time))))
    (setf (gethash name *perspectives*) persp)
    persp))

(defun perspective-delete (name)
  "Delete perspective with NAME. Cannot delete current perspective."
  (let ((persp (find-perspective name)))
    (unless persp
      (editor-error "Perspective '~A' does not exist" name))
    (when (eq persp *current-perspective*)
      (editor-error "Cannot delete current perspective"))
    (remhash name *perspectives*)))

(defun update-perspective-access-time (persp)
  "Update the last-accessed time for PERSP."
  (setf (perspective-last-accessed persp) (get-universal-time)))

;;; Buffer Management

(defun perspective-add-buffer (persp buffer)
  "Add BUFFER to PERSP's buffer list if not already present."
  (pushnew buffer (perspective-buffers persp)))

(defun perspective-remove-buffer (persp buffer)
  "Remove BUFFER from PERSP's buffer list."
  (setf (perspective-buffers persp)
        (remove buffer (perspective-buffers persp))))

(defun perspective-has-buffer-p (persp buffer)
  "Check if PERSP contains BUFFER."
  (member buffer (perspective-buffers persp)))

(defun current-perspective-buffers ()
  "Get the buffer list for the current perspective."
  (perspective-buffers (current-perspective)))

;;; Frame Association

(defun perspective-frame (persp)
  "Get the frame ID associated with PERSP."
  (perspective-frame-id persp))

(defun set-perspective-frame (persp frame-id)
  "Associate PERSP with FRAME-ID."
  (setf (perspective-frame-id persp) frame-id))

;;; Perspective Switching

(defun activate-perspective (persp)
  "Switch to PERSP, swapping buffer lists and frames."
  ;; Save current perspective's buffer list
  (when *current-perspective*
    (setf (perspective-buffers *current-perspective*)
          (buffer-list)))

  ;; Update access time
  (update-perspective-access-time persp)

  ;; Switch to new perspective
  (setf *current-perspective* persp)

  ;; Directly set the buffer list (not temporary swap)
  (setf (lem/buffer/buffer-list-manager::buffer-list-manager-buffers
         (lem/buffer/buffer-list-manager:buffer-list-manager))
        (perspective-buffers persp))

  ;; Switch to perspective's frame if it has one
  (when (perspective-frame-id persp)
    (handler-case
        (lem/frame-multiplexer:frame-multiplexer-switch (perspective-frame-id persp))
      (error (e)
        ;; Frame might not exist anymore, clear the association
        (setf (perspective-frame-id persp) nil))))

  ;; Switch to a buffer in this perspective
  (let ((buffers (perspective-buffers persp)))
    (when buffers
      (switch-to-buffer (first buffers) nil nil))))

(defun perspective-switch (name)
  "Switch to perspective with NAME."
  (let ((persp (find-perspective name)))
    (unless persp
      (editor-error "Perspective '~A' does not exist" name))
    (activate-perspective persp)))

;;; Commands

(define-command perspective-new-command (name) ((:string "New perspective name: "))
  "Create a new perspective."
  (let ((persp (perspective-new name)))
    (activate-perspective persp)
    (message "Created perspective: ~A" name)))

(define-command perspective-kill-command () ()
  "Kill the current perspective and switch to another."
  (let* ((current-name (perspective-name (current-perspective)))
         (other-persps (remove current-name (all-perspective-names) :test #'string=)))
    (when (null other-persps)
      (editor-error "Cannot kill the last perspective"))

    ;; Kill all buffers in current perspective
    (dolist (buffer (perspective-buffers (current-perspective)))
      (delete-buffer buffer))

    ;; Delete perspective
    (remhash current-name *perspectives*)

    ;; Switch to another perspective
    (let ((next-persp (find-perspective (first other-persps))))
      (activate-perspective next-persp)
      (message "Killed perspective '~A', switched to '~A'" current-name (first other-persps)))))

(define-command perspective-rename-command (new-name) ((:string "New name: "))
  "Rename the current perspective."
  (let* ((persp (current-perspective))
         (old-name (perspective-name persp)))
    (when (perspective-exists-p new-name)
      (editor-error "Perspective '~A' already exists" new-name))

    ;; Remove old entry, add new one
    (remhash old-name *perspectives*)
    (setf (perspective-name persp) new-name)
    (setf (gethash new-name *perspectives*) persp)

    ;; Update frame name if associated
    (when (perspective-frame-id persp)
      (lem/frame-multiplexer:frame-multiplexer-rename new-name))

    (message "Renamed perspective from '~A' to '~A'" old-name new-name)))

(define-command perspective-list-command () ()
  "List all perspectives."
  (let* ((persps (all-perspectives))
         (sorted (sort persps #'> :key #'perspective-last-accessed))
         (current-name (perspective-name (current-perspective)))
         (lines (loop for persp in sorted
                      for name = (perspective-name persp)
                      for num-buffers = (length (perspective-buffers persp))
                      for current-marker = (if (string= name current-name) "*" " ")
                      collect (format nil "~A ~A (~D buffer~:P)"
                                      current-marker name num-buffers))))
    (if lines
        (message "Perspectives:~%~{~A~%~}" lines)
        (message "No perspectives"))))

;;; Initialization

(defun initialize-perspectives ()
  "Initialize the perspective system with a default perspective."
  (unless (perspective-exists-p *default-perspective-name*)
    (let ((default-persp (perspective-new *default-perspective-name*)))
      ;; Add all current buffers to default perspective
      (dolist (buffer (buffer-list))
        (perspective-add-buffer default-persp buffer))
      (setf *current-perspective* default-persp))))

;; Initialize on load
(initialize-perspectives)

;;; Package export for lem-user

(in-package :lem-user)

;; Re-export commands for convenience
(import '(lem-perspectives:perspective-new-command
          lem-perspectives:perspective-kill-command
          lem-perspectives:perspective-rename-command
          lem-perspectives:perspective-list-command))

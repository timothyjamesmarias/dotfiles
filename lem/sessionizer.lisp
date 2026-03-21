;;; sessionizer.lisp - Project sessionizer for Lem
;;; Inspired by tmux-sessionizer but for the Lisp machine paradigm

(defpackage :lem-sessionizer
  (:use :cl :lem)
  (:export :sessionizer
           :*sessionizer-search-paths*))

(in-package :lem-sessionizer)

;;; Configuration

(defvar *sessionizer-search-paths*
  (list (merge-pathnames "code/" (user-homedir-pathname))
        (merge-pathnames "work/" (user-homedir-pathname))
        (merge-pathnames "projects/" (user-homedir-pathname)))
  "List of directories to search for projects.")

(defvar *sessionizer-ignored-dirs*
  '(".git" "node_modules" "target" ".qlot" "dist" "build" ".cache")
  "Directory names to ignore when searching for projects.")

(defvar *sessionizer-max-depth* 3
  "Maximum depth to search for projects.")

;;; Project Detection (Projectile-compatible markers)

(defvar *project-markers*
  '(".git" ".hg" "_FOSSIL_" ".bzr" "_darcs"
    ".projectile" ".project" "Makefile" "TAGS"
    "configure.ac" "package.json" "Cargo.toml"
    "pom.xml" "build.gradle" "Gemfile" "mix.exs"))

(defun project-root-p (directory)
  "Check if DIRECTORY is a project root."
  (loop for marker in *project-markers*
        thereis (or (probe-file (merge-pathnames marker directory))
                    (uiop:directory-exists-p (merge-pathnames marker directory)))))

;;; Project Discovery

(defun find-projects-in-directory (directory &optional (depth 0))
  "Recursively find all project directories under DIRECTORY up to *sessionizer-max-depth*."
  (when (and directory (uiop:directory-exists-p directory))
    (let ((projects '()))
      ;; Check if current directory is a project
      (when (project-root-p directory)
        (push directory projects))

      ;; Recurse into subdirectories if not at max depth
      (when (< depth *sessionizer-max-depth*)
        (dolist (entry (uiop:subdirectories directory))
          (let ((dirname (car (last (pathname-directory entry)))))
            ;; Skip ignored directories
            (unless (member dirname *sessionizer-ignored-dirs* :test #'string=)
              (setf projects (nconc projects (find-projects-in-directory entry (1+ depth))))))))

      projects)))

(defun discover-projects ()
  "Discover all projects in configured search paths."
  (let ((projects '()))
    (dolist (path *sessionizer-search-paths*)
      (when (uiop:directory-exists-p path)
        (setf projects (nconc projects (find-projects-in-directory path)))))
    (remove-duplicates projects :test #'equal)))

(defun project-display-name (project-path)
  "Generate a display name for PROJECT-PATH."
  (let ((path-components (pathname-directory project-path)))
    (if path-components
        (car (last path-components))
        (namestring project-path))))

;;; Perspective Integration

(defun get-current-frame-id ()
  "Get the current frame ID from the frame multiplexer."
  (let ((vf (gethash (lem:implementation) lem/frame-multiplexer::*virtual-frame-map*)))
    (when vf
      (lem/frame-multiplexer::find-frame-id
       vf
       (lem/frame-multiplexer::virtual-frame-current vf)))))

;;; Project Switching

(defun switch-to-project (project-path)
  "Switch to or create a perspective for PROJECT-PATH."
  (let* ((project-name (project-display-name project-path))
         (existing-persp (lem-perspectives:find-perspective project-name)))

    (if existing-persp
        ;; Switch to existing perspective
        (progn
          (lem-perspectives::activate-perspective existing-persp)
          (message "Switched to project: ~A" project-name))
        ;; Create new perspective for this project
        (progn
          ;; Create new frame with dedicated buffer list
          (lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)

          ;; Rename frame to project name
          (lem/frame-multiplexer:frame-multiplexer-rename project-name)

          ;; Create new perspective
          (let ((persp (lem-perspectives::perspective-new project-name :root project-path)))
            ;; Associate perspective with the new frame
            (lem-perspectives::set-perspective-frame persp (get-current-frame-id))

            ;; Change to project directory
            (uiop:chdir project-path)

            ;; Try to open a default file
            (let ((default-file (or (probe-file (merge-pathnames "README.md" project-path))
                                    (probe-file (merge-pathnames "readme.md" project-path))
                                    (probe-file (merge-pathnames "README" project-path)))))
              (when default-file
                (find-file default-file))

              ;; Add the buffer to perspective
              (when (current-buffer)
                (lem-perspectives::perspective-add-buffer persp (current-buffer)))

              ;; Activate the perspective
              (lem-perspectives::activate-perspective persp)

              (message "Created project: ~A" project-name)))))))

;;; Main Command

(define-command sessionizer () ()
  "Fuzzy find and switch to a project."
  (let* ((projects (discover-projects))
         (project-names (mapcar (lambda (p)
                                  (cons (project-display-name p)
                                        (namestring p)))
                                projects)))

    (if (null projects)
        (message "No projects found in configured search paths")
        (let* ((completion-items (mapcar #'car project-names))
               (selected-name (prompt-for-string
                              "Select project: "
                              :completion-function (lambda (x)
                                                    (completion-strings x completion-items))
                              :test-function (lambda (name)
                                             (member name completion-items :test #'string=)))))
          (when selected-name
            (let ((selected-path (cdr (assoc selected-name project-names :test #'string=))))
              (when selected-path
                (switch-to-project (pathname selected-path)))))))))

;;; Package export for lem-user
(in-package :lem-user)

;; Re-export the main command for convenience
(import 'lem-sessionizer:sessionizer)

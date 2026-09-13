;;; modules/database.el --- Database interaction via ejc-sql -*- lexical-binding: t; -*-

(after! ejc-sql
  ;; Auto-install JDBC drivers on first connection
  (setq ejc-use-maven t)

  ;; Keep result sets readable
  (setq ejc-result-table-impl 'orgtbl-mode)

  ;; Completion and eldoc in connected SQL buffers. ejc only ships company
  ;; and auto-complete backends (neither installed here), so this is a
  ;; native capf over the same candidate layer (ejc-completion-common).
  (require 'ejc-completion-common)
  (defun +ejc-capf ()
    "Complete schema objects and SQL keywords from ejc-sql's cache."
    (when (bound-and-true-p ejc-db)
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (list (or (car bounds) (point))
              (or (cdr bounds) (point))
              (completion-table-dynamic
               (lambda (_)
                 (append (ejc-get-ansi-sql-words)
                         (ejc-get-keywords)
                         (ejc-owners-candidates)
                         (ejc-tables-candidates)
                         (ejc-views-candidates)
                         (ejc-colomns-candidates))))
              :exclusive 'no))))
  (add-hook 'ejc-sql-mode-hook
            (defun +ejc--setup-completion-h ()
              (add-hook 'completion-at-point-functions #'+ejc-capf nil t)
              (ejc-eldoc-setup))))

;; ejc features (completion, statement eval) activate via this minor mode.
(add-hook 'sql-mode-hook #'ejc-sql-mode)

(defun +ejc-show-tables ()
  "List application tables, excluding system schemas.
Upstream `ejc-show-tables-list' dumps every schema including
pg_catalog/information_schema."
  (interactive)
  (ejc-check-connection)
  (ejc-eval-user-sql
   "SELECT table_schema, table_name
    FROM information_schema.tables
    WHERE table_schema NOT IN
      ('pg_catalog', 'information_schema', 'mysql', 'performance_schema', 'sys')
    ORDER BY table_schema, table_name"
   :rows-limit 0
   :fetch-size 0
   :column-width-limit 0
   :display-result t))

;; Per-project connections are defined in .dir-locals.el:
;;
;;   ((nil . ((eval . (ejc-create-connection
;;             "my-project-primary"
;;             :dependencies [[org.postgresql/postgresql "42.6.0"]]
;;             :dbtype "postgresql"
;;             :host "localhost"
;;             :port "5432"
;;             :dbname "my_db"
;;             :user "postgres"
;;             :password "postgres"))
;;
;;           (eval . (ejc-create-connection
;;             "my-project-analytics"
;;             :dependencies [[mysql/mysql-connector-java "5.1.6"]]
;;             :dbtype "mysql"
;;             :host "localhost"
;;             :port "3306"
;;             :dbname "analytics"
;;             :user "root"
;;             :password "root")))))
;;
;; Then connect with: M-x ejc-connect RET my-project-primary RET

(defun +ejc-connect-project ()
  "Connect to the ejc connection matching the current project's name.

Connections are registered by the project's .dir-locals.el on file
visit, named after the project directory (see ejc-sql-setup skill).
Prompts only when the project has several connections; falls back to
plain `ejc-connect' when it has none."
  (interactive)
  (require 'ejc-sql)
  ;; The dir-locals eval of `ejc-create-connection' fails silently when the
  ;; file was visited before ejc-sql loaded (no autoload upstream), leaving
  ;; `ejc-connections' empty. Re-run local variables now that it's defined.
  (if buffer-file-name
      (hack-local-variables)
    (hack-dir-local-variables-non-file-buffer))
  (let* ((project (doom-project-name))
         (matches (seq-filter (lambda (c) (string-prefix-p project (car c)))
                              ejc-connections)))
    (cond
     ((null matches) (call-interactively #'ejc-connect))
     ((cdr matches) (ejc-connect (completing-read "Connection: " (mapcar #'car matches))))
     (t (ejc-connect (caar matches))))))

(map! :leader
      (:prefix ("d" . "database")
       :desc "Connect (project)" "c" #'+ejc-connect-project
       :desc "Connect (any)"     "a" #'ejc-connect
       :desc "Connect (interactive)" "C" #'ejc-connect-interactive
       :desc "SQL scratch"       "s" #'ejc-get-temp-editor-buffer
       :desc "Eval SQL at point" "e" #'ejc-eval-user-sql-at-point
       :desc "Eval SQL region"   "r" #'ejc-eval-user-sql-region
       :desc "App tables"        "t" #'+ejc-show-tables
       :desc "All tables (raw)"  "T" #'ejc-show-tables-list
       :desc "Describe table"    "d" #'ejc-describe-table
       :desc "Result buffer"     "o" #'ejc-show-last-result
       :desc "Prev result"       "[" #'ejc-show-prev-result
       :desc "Next result"       "]" #'ejc-show-next-result
       :desc "Cancel query"      "k" #'ejc-cancel-query
       :desc "Statement log"     "l" #'ejc-open-log
       :desc "Disconnect"        "q" #'ejc-quit-connection))

;; Statement motions, matching other ]x/[x structural motions.
(map! :map sql-mode-map
      :n "]s" #'ejc-next-sql
      :n "[s" #'ejc-previous-sql)

;; Localleader in SQL buffers: same actions without reaching for SPC d.
(map! :localleader
      :map sql-mode-map
      :desc "Eval at point"    "e" #'ejc-eval-user-sql-at-point
      :desc "Eval region"      "r" #'ejc-eval-user-sql-region
      :desc "App tables"       "t" #'+ejc-show-tables
      :desc "Describe table"   "d" #'ejc-describe-table
      :desc "Last result"      "o" #'ejc-show-last-result
      :desc "Format statement" "f" #'ejc-format-sql-at-point)

;; Evil-style eval: gr runs the statement at point, matching eval operators
;; elsewhere in the config.
(map! :map sql-mode-map
      :n "gr" #'ejc-eval-user-sql-at-point)

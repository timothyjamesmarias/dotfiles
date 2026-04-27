;;; modules/database.el --- Database interaction via ejc-sql -*- lexical-binding: t; -*-

(after! ejc-sql
  ;; Auto-install JDBC drivers on first connection
  (setq ejc-use-maven t)

  ;; Keep result sets readable
  (setq ejc-result-table-impl 'orgtbl-mode))

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

(map! :leader
      (:prefix ("d" . "database")
       :desc "Connect"          "c" #'ejc-connect
       :desc "Connect (interactive)" "C" #'ejc-connect-interactive
       :desc "Eval SQL at point" "e" #'ejc-eval-user-sql-at-point
       :desc "Eval SQL region"  "r" #'ejc-eval-user-sql-region
       :desc "Show tables"      "t" #'ejc-show-tables-list
       :desc "Describe table"   "d" #'ejc-describe-table
       :desc "Result buffer"    "o" #'ejc-show-last-result
       :desc "Disconnect"       "q" #'ejc-quit-connection))

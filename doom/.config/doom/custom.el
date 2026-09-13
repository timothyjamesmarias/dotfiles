;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval when (fboundp 'ejc-create-connection)
      (ejc-create-connection "starman-source-gc" :dependencies
       [[org.postgresql/postgresql "42.7.4"]] :dbtype "postgresql" :host
       "127.0.0.1" :port "5432" :dbname "starman_ground_control_development"
       :user "postgres" :password "postgres")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

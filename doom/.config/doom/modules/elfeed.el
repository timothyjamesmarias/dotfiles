;;; modules/elfeed.el --- RSS feed reader configuration -*- lexical-binding: t; -*-

(after! elfeed
  (setq elfeed-search-filter "@1-week-ago +unread"
        elfeed-search-title-max-width 80
        elfeed-show-entry-switch #'pop-to-buffer))

(after! elfeed-org
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" org-directory))))

(map! :leader
      (:prefix ("r" . "rss")
       :desc "Open elfeed" "s" #'elfeed))

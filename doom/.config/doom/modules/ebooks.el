;;; modules/ebooks.el --- Ebook reading -*- lexical-binding: t; -*-

;; File associations (top-level so it's available before autoloads)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; --- EPUB (nov.el) ---
(after! nov
  (setq nov-text-width 80)

  (defun +tim/nov-setup ()
    "Configure buffer-local settings for comfortable EPUB reading."
    (face-remap-add-relative 'default
                             :family "Charter"
                             :height 160)
    (face-remap-add-relative 'variable-pitch
                             :family "Charter"
                             :height 160)
    (setq-local line-spacing 6)
    (visual-line-mode 1)
    (display-line-numbers-mode -1)
    (setq-local cursor-type 'bar))

  (add-hook 'nov-mode-hook #'+tim/nov-setup)

  (map! :map nov-mode-map
        :n "n"   #'nov-next-document
        :n "p"   #'nov-previous-document
        :n "t"   #'nov-goto-toc
        :n "RET" #'nov-browse-url
        :n "]]"  #'nov-next-document
        :n "[["  #'nov-previous-document
        :n "q"   #'kill-current-buffer
        :n "d"   #'nov-scroll-up
        :n "u"   #'nov-scroll-down))

;; --- PDF (supplements Doom's :tools pdf module) ---
(after! pdf-tools
  (setq pdf-view-use-scaling t)
  (setq pdf-view-midnight-colors '("#c5c8c6" . "#1d1f21"))
  (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

;; --- Convenience functions ---
(defun +tim/open-book ()
  "Open an ebook file (epub/pdf) using completing-read."
  (interactive)
  (let ((default-directory (or (and (file-directory-p "~/Books/") "~/Books/")
                               "~/")))
    (find-file (read-file-name "Open book: " default-directory nil t nil
                               (lambda (f)
                                 (or (file-directory-p f)
                                     (string-match-p "\\.\\(epub\\|pdf\\)\\'" f)))))))

(defun +tim/reading-mode ()
  "Toggle zen mode tuned for book reading."
  (interactive)
  (if (bound-and-true-p writeroom-mode)
      (writeroom-mode -1)
    (writeroom-mode +1)))

;; --- Leader bindings ---
(map! :leader
      (:prefix ("r" . "reader")
       :desc "Open ebook file"    "o" #'+tim/open-book
       :desc "Reading mode (zen)" "z" #'+tim/reading-mode))

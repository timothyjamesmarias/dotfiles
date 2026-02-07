;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-
;;
;; Author: Tim Marias
;; Description: Early initialization for performance and package bootstrap
;;
;; This file runs before package.el and GUI initialization.
;; Used for performance optimizations and straight.el bootstrap.
;;
;;; Commentary:
;; Emacs 27+ loads this file before init.el and package initialization.
;; Perfect for performance tweaks and preventing unwanted UI elements.
;;
;;; Code:

;; ========================================
;; Performance Optimizations
;; ========================================

;; Increase garbage collection threshold during startup
;; Will be reset in init.el after startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent premature redisplay during startup
(setq-default inhibit-redisplay t
              inhibit-message t)

;; Restore normal values after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024) ; 16MB
                  gc-cons-percentage 0.1
                  inhibit-redisplay nil
                  inhibit-message nil)))

;; Prefer loading newest compiled files
(setq load-prefer-newer t)

;; Don't create lockfiles
(setq create-lockfiles nil)

;; ========================================
;; UI Cleanup - Remove Chrome
;; ========================================

;; Disable unnecessary UI elements before they're loaded
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable startup screens
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; ========================================
;; Package System Configuration
;; ========================================

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; ========================================
;; Straight.el Bootstrap
;; ========================================

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package via straight
(straight-use-package 'use-package)

;; Configure straight.el to integrate with use-package
(setq straight-use-package-by-default t)

;; Load Org early to prevent version mismatch
;; This must be done before any package that depends on Org
(straight-use-package 'org)

;; ========================================
;; Native Compilation (if available)
;; ========================================

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Silence compiler warnings as they are pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Set the right directory to store the native compilation cache
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory)))

;; ========================================
;; Frame Settings
;; ========================================

;; Start maximized
(push '(fullscreen . maximized) default-frame-alist)

;; Transparency settings (optional)
;; (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; ========================================
;; macOS Modifier Keys
;; ========================================

;; Set Command as Meta, disable Option (so Option can type special chars)
;; This must be set early to work properly
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none
        mac-right-option-modifier 'none))

;;; early-init.el ends here

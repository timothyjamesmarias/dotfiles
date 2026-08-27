;;; cli.el -*- lexical-binding: t; -*-
;; Loaded by Doom's CLI (doom sync, doctor, etc.), which runs in batch mode.
;; astro-ts-mode's autoloads call `treesit-ready-p' at top level; treesit isn't
;; preloaded in batch, and the resulting void-function error silently aborts
;; profile init generation (leaving init.<version>.el unwritten, which breaks
;; Emacs startup with "Doom hasn't been initialized yet").
(require 'treesit nil t)

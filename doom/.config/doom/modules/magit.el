;;; modules/magit.el --- Git client configuration -*- lexical-binding: t; -*-

(after! magit
  (setq magit-list-refs-sortby "-committerdate"))

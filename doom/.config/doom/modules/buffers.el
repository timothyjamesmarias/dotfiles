;;; modules/buffers.el --- Tab-bar buffer cycling -*- lexical-binding: t; -*-

(map! :n "C-n" #'centaur-tabs-forward-tab
      :n "C-p" #'centaur-tabs-backward-tab)

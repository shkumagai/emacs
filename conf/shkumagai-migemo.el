;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Migemo: incremental search of Roman script
(when (and (executable-find "cmigemo")
	   (require 'migemo nil t))
  ;; use cmigemo
  (setq migemo-command "cmigemo")
  ;; command line options
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  ;; path to dictionary
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  ;; required setting in cmigemo
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  ;; cache setting
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1000)
  (setq migemo-coding-system 'utf-8-unix)
  ;; initialize migemo
  (migemo-init))
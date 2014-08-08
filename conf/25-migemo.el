;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; How to setting up on MacOSX
;; ---------------------------
;;
;; .. note::
;;    This extention requires cmigemo executable. So, you need to install
;;    C/Migemo if you want to use.
;;
;; Install C/Migemo into your environment according to installation procedure
;; of this article.
;;
;; Refer to: http://samurai20000.hatenablog.com/entry/20100907/1283791433
;;
;; Install migemo.el from gist.
;;
;; gist: http://gist.github.com/457761

;; Migemo: incremental search of Roman script
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  ;; use cmigemo
  (setq migemo-command "cmigemo")
  ;; command line options
  (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  ;; path to dictionary
  (setq migemo-dictionary "/usr/share/migemo/migemo-dict")
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

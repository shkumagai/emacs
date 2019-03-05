;;; 23-migemo.el ---  -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:

;; How to setting up on MacOSX
;; ---------------------------
;;
;; .. note::
;;    This extention requires cmigemo executable.  So, you need to install
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

;;; Code:

;; Migemo: incremental search of Roman script
(use-package migemo
  :if (executable-find "cmigemo")
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  ;; cache setting
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1000)
  (setq migemo-coding-system 'utf-8-unix)

  (cond
   ((eq system-type 'darwin)
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
    )
   ((eq system-type 'gnu/linux)
    (setq migemo-options '("-q" "--emacs" "-i" "\a"))
    (setq migemo-dictionary "/usr/share/migemo/migemo-dict")
    ))

  (migemo-init)
  )

;;; 23-migemo.el ends here

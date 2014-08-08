;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Go
(add-to-list 'load-path "/usr/share/emacs/site-list/go" t)
(when (require 'go-mode-load nil t)
  (add-hook 'before-save-hook 'gofmt-before-save))

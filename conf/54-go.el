;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Go
(add-to-list 'load-path "/opt/local/go/misc/emacs" t)
(when (require 'go-mode-load nil t)
  (add-hook 'before-save-hook 'gofmt-before-save))

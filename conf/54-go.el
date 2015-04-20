;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Go
(when (require 'go-mode-load nil t)
  (add-hook 'before-save-hook 'gofmt-before-save))

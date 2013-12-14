;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; JavaScript
(when (require 'js2-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\(_t\\)\\'" . js2-mode))
  )

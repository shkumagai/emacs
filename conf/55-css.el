;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; CSS
(when (require 'css-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.css\\(_t\\)\\'" . css-mode))
  )

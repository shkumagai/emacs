;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Groovy
(when (require 'groovy-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
  (add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))
  )

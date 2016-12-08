;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; JavaScript
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\(_t\\)\\'" . js2-mode))
  )

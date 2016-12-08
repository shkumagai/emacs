;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; CSS
(use-package css-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.css\\(_t\\)\\'" . css-mode))
  )

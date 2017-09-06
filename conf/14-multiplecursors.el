;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/skip-all-like-this))
  )

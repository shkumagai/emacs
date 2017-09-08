;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; undo-tree: visualize undo branches
;; M-x package-install undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; undohist: enable undo on closed buffer
;; download from: http://cx4a.org/pub/undohist.el
(use-package undohist
  :config
  (undohist-initialize))

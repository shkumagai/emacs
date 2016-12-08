;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

(use-package anything-startup)
(use-package anything-exuberant-ctags
  :if (require 'anything nil t)
  :bind (("C-M-@" . anything-exuberant-ctags-select-from-here))
  )

;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Anything
(require 'anything-startup)

;; anything-exuberant-ctags
(when (require 'anything nil t)
  (require 'anything-exuberant-ctags)
  (define-key global-map (kbd "C-M-@") 'anything-exuberant-ctags-select-from-here))

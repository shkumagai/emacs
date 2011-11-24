;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Color Theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-arjen)))

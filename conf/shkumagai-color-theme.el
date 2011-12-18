;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; How to setting up on MacOSX
;;
;; Install color-theme-solarized
;;    I use solarized color-theme thrugh color-theme.el. If you want to
;;    know how to install, see https://github.com/altercation/solarized

;; Color Theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     ;; (color-theme-arjen)
     (color-theme-solarized-dark)
     ))

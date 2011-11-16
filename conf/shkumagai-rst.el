;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; reStructuredText
(when (require 'rst nil t)
  (setq auto-mode-alist
        (cons '("\\.re?st$" . rst-mode) auto-mode-alist))
  (add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil))))
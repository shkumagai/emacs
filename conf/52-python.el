;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Python
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))

;; reStructuredText
(when (require 'rst nil t)
  (setq auto-mode-alist
        (cons '("\\.re?st$" . rst-mode) auto-mode-alist))
  (add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil))))

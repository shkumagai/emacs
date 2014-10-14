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

;; flymake-python-pyflakes
(when (require 'flymake-python-pyflakes nil t)
  (setq flymake-python-pyflakes-executable
        (replace-regexp-in-string "\n+$" "" (shell-command-to-string "which flake8")))
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load))

;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; smartchr: sycle snippet
(when (require 'smartchr nil t)
  (defun cperl-mode-hooks ()
    (define-key cperl-mode-map (kbd "=") (smartchr '("=" " = " " == " " => "))))
  (add-hook 'cperl-mode-hook 'cperl-mode-hooks)
  (defun css-mode-hooks ()
    (define-key cssm-mode-map (kbd ":") (smartchr '(": " ":"))))
  (add-hook 'css-mode-hook 'css-mode-hooks))

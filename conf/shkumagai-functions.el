;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; global
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; lisp-mode-hook
(defun lisp-mode-hooks ()
  "lisp-mode-hooks"
  (require 'eldoc)
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p t)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'lisp-mode-hooks)
(add-hook 'lisp-interaction-mode-hook 'lisp-mode-hooks)
(add-hook 'ielm-mode-hook 'lisp-mode-hooks)

;; smartchr: sycle snippet
(when (require 'smartchr nil t)
  (defun cperl-mode-hooks ()
    (define-key cperl-mode-map (kbd "=") (smartchr '("=" " = " " == " " => "))))
  (add-hook 'cperl-mode-hook 'cperl-mode-hooks)
  (defun css-mode-hooks ()
    (define-key cssm-mode-map (kbd ":") (smartchr '(": " ":"))))
  (add-hook 'css-mode-hook 'css-mode-hooks))

;; wdired: enable file name edit directly in wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)


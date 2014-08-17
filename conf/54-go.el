;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Go
(when (eq window-system 'ns)
  (add-to-list 'load-path "/opt/local/share/emacs/site-lisp" t))

(when (eq window-system 'x)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/go" t))


(when (require 'go-mode-load nil t)
  (add-hook 'before-save-hook 'gofmt-before-save))

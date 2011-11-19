;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; auto-complete
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "/elisp/ac-dict"))
  (add-to-list 'ac-modes 'erlang-mode)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))
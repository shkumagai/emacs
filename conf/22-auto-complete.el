;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; How to seting up on MacOSX
;;
;; Install auto-complete package as below.
;;
;; M-x package install auto-complete
;;
;; And then, add elisp snippet as below to your .emacs or init.el.

;; For more informations, refer to official documentation.
;;
;; http://cx4a.org/software/auto-complete/index.html    (en)
;; http://cx4a.org/software/auto-complete/index.ja.html (ja)

;; auto-complete
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "/ac-dict"))
  (add-to-list 'ac-modes 'erlang-mode)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

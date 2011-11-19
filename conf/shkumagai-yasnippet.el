;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; How to seting up on MacOSX

;; Install YASnippet package in your own environment according to installation
;; direction in the document.
;;
;; Project site: http://capitaomorte.github.com/yasnippet/

;; And then, add elisp snippet as below to your .emacs or init.el.

;; Yet another snippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat user-emacs-directory "snippets"))

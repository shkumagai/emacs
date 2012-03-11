;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; How to setting up git-emacs on MacOSX
;;   I'm using git-emacs which forked by 'mhayashi1120'.
;;
;;   - Clone from github.
;;
;;     $ cd path/to/your/elisp/dir
;;     $ git clone http://github.com/mhayashi1120/git-emacs.git

(add-to-list 'load-path (concat user-emacs-directory "elisp/git-emacs"))
(when (require 'git-emacs nil t))

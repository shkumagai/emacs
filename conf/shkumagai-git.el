;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; How to setting up git-emacs on MacOSX
;;   I'm using git-emacs which forked by 'mhayashi1120'.
;;
;;   - Clone from github.
;;
;;     $ cd path/to/your/elisp/dir
;;     $ git clone http://github.com/mhayashi1120/git-emacs.git

;; (add-to-list 'load-path (concat user-emacs-directory "elisp/git-emacs"))
;; (when (require 'git-emacs nil t))

;; How to setting up magit on MacOSX
;;
;;  1. Clone from github.
;;
;;     $ cd path/to/work/dir
;;     $ git clone https://github.com/magit/magit.git
;;
;;  2. Run make & make install
;;
;;     $ make
;;     $ sudo make install
;;
;;  Note:
;;    libraries will be installed into /usr/local as default.
;;    If you want to install libraries into where you specified,
;;    you need to pass --prefix option to the make command.

(when (require 'magit nil t))
(global-set-key (kbd "C-c C-g") 'magit-status)
;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; How to setting up on MacOSX
;;
;;   - Download from sourceforge
;;
;;     http://sourceforge.jp/projects/hatena-diary-el/downloads/49483/hatena-diary210.tar.gz/
;;
;;     $ tar zxf hatena-diary210.tar.gz
;;     $ mv hatena-diary ~/.emacs.d/elisp/.

(add-to-list 'load-path (concat user-emacs-directory "elisp/hatena-diary"))
(when (require 'hatena-diary-mode nil t)
  (setq hatena-usrid "navyfox_sh")
  (setq hatena-twitter-flag nil))

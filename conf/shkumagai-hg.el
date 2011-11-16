;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Mercurial
(push "/opt/local/share/mercurial/contrib" load-path)
(load-library "mercurial")
(load-library "mq")
;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

(use-package exec-path-from-shell
  :if (not (eq window-system 'w32))
  :config
  (exec-path-from-shell-initialize))

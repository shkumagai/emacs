;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

(use-package ansible
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible)))
  )

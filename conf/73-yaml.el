;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode; nil -*-

;; YAML
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
  )

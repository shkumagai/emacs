;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

(use-package ctags
  :bind (("<f5>" . ctags-create-or-update-tags-table))
  :config
  (setq tags-revert-without-query t)
  ;; Command-line to call `ctags'.
  ;; (setq ctags-command "/opt/local/bin/ctags -R -e ")
  ;; Comment out when anything-exuberant-ctags.el not in use.
  (setq ctags-command "ctags -e -R --fields=\"+afikKlmnsSzt\" ")
  )

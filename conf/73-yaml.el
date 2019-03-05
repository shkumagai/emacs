;;; 73-yaml.el --- -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode; nil -*-

;;; Commentary:
;;; Code:

;; YAML
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
  )

;;; 73-yaml.el ends here

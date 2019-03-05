;;; 75-rest.el --- -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:
;;; Code:

(use-package rst
  :config
  (add-to-list 'auto-mode-alist '("\\.rse?t$" . rst-mode))
  )

;;; 75-rest.el ends here

;;; 53-js.el --- -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:
;;; Code:

;; JavaScript
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\(_t\\)\\'" . js2-mode))
  )

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  )

;;; 53-js.el ends here

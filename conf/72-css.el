;;; 72-css.el --- -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:
;;; Code:

;; CSS
(use-package css-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.css\\(_t\\)\\'" . css-mode))
  )

;;; 72-css.el ends here

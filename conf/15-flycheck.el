;;; 15-flycheck.el --- -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:
;;; Code:

;; flycheck
(use-package flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; 15-flycheck.el ends here

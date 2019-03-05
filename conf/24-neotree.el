;;; 24-neotree.el --- -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:
;;; Code:

(use-package neotree
  :config
  (setq neo-theme 'ascii)
  (setq neo-persist-show t)
  (setq neo-smart-open t)
  (global-set-key "\C-o" 'neotree-toggle))

;;; 24-neotree.el ends here

;;; 03-exec-path.el --- -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :if (not (eq window-system 'w32))
  :config
  (exec-path-from-shell-initialize))

;;; 03-exec-path.el ends here

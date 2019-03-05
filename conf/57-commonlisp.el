;;; 57-commonlisp.el --- -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:

;; Setup load-path, autoloads and your Lisp system.
;; (add-to-list 'load-path "~/.emacs.d/elisp/slime")

;;; Code:

(use-package slime-autoloads
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-repl slime-fancy slime-banner slime-indentation slime-company))
  )

;;; 57-commonlisp.el ends here

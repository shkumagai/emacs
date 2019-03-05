;;; 74-markdown.el --- -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode; nil -*-

;;; Commentary:
;;; Code:

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; 74-markdown.el ends here

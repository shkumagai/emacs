;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; auto-install.el
(when (require 'auto-install nil t)
  (setq auto-install-directory (expand-file-name
                                (concat user-emacs-directory "elisp/")))
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))
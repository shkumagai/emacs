;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

(use-package auto-install
  :config
  (setq auto-install-directory (expand-file-name
                                (concat user-emacs-directory "elisp/")))
  (auto-install-compatibility-setup))

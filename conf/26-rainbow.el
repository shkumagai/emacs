;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

(use-package rainbow-mode
  :config
  (setq rainbow-html-colors nil)
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  )

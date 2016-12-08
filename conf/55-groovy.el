;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Groovy
(use-package groovy-mode
  :config
  (add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
  (add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

  (add-hook 'groovy-mode-hook
            '(lambda ()
               (use-package groovy-electric)
               (groovy-electric-mode))))

;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Coffee script
(when (require 'coffee-mode nil t)
  (defun coffee-custom ()
    "coffee-mode-hook"
    (and (set (make-local-variable 'tab-width) 2)
         (set (make-local-variable 'coffee-tab-width) 2))
    )
  (add-hook 'coffee-mode-hook
            '(lambda () (coffee-custom))))

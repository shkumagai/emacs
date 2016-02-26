;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Python
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))

;; jedi
(when (require 'jedi-core nil t)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-to-list 'company-backends 'company-jedi)
  (add-hook 'python-mode-hook 'jedi:setup))

;; ;; reStructuredText
;; (when (require 'rst nil t)
;;   (setq auto-mode-alist
;;         (cons '("\\.re?st$" . rst-mode) auto-mode-alist))
;;   (add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil))))

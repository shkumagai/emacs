;;; 22-company.el --- -*- mode: emacs-list; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:
;;; Code:

(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0)  ; default: 0.5
  (setq company-minimum-prefix-length 3)  ; default: 4
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  )

;;; 22-company.el ends here

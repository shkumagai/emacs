;; -*- mode: emacs-list; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; company
(when (require 'company nil t)
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t))

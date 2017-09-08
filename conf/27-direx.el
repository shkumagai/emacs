;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; popwin
(use-package popwin
  :config
  (setq display-buffer-function 'popwin:display-buffer))

;; direx
(use-package direx
  :config
  (setq direx:leaf-icon "  "
        direx:open-icon "- "
        direx:closed-icon "+ ")
  (push '(direx:direx-mode :position left :width 40 :dedicated t)
        popwin:special-display-config))

;; WORKAROUND: This setting MUST be placed after Helm settings.
;; Otherwise, "C-x C-j" binding override by helm(?). I don't know why.
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

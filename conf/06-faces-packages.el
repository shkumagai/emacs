;;; 06-faces-packages.el --- -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:
;;; Code:

;; Whitespace mode
(use-package whitespace
  :bind (("C-x w" . global-whitespace-mode))
  :config
  (setq whitespace-style '(face
                           trailing
                           tabs
                           spaces
                           empty
                           tab-mark
                           space-mark
                           ))

  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (setq whitespace-space-regexp "\\( +\\|\u3000+\\)")
  (global-whitespace-mode 1)

  (defvar my/bg-color "#2d3743")
  (set-face-attribute 'whitespace-trailing nil
                      :background my/bg-color
                      :foreground "DeepPink"
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :background my/bg-color
                      :foreground "LightSkyBlue"
                      :underline t)
  (set-face-attribute 'whitespace-space nil
                      :background my/bg-color
                      :foreground "GreenYellow"
                      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
                      :background my/bg-color
                      ))

;;; 06-faces-packages.el ends here

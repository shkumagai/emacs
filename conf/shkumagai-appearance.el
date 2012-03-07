;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Visibility
(setq inhibit-startup-screen t)
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(blink-cursor-mode t)
(menu-bar-mode 0)
(display-time)
(column-number-mode 1)
(setq default-frame-alist
      '((width . 160)
        (height . 60)
        (top . 10)
        (left . 0)
        ))

;; show absolute path on title bar
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))
;; parentheses
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; change background color in resion
(set-face-background 'region "darkgreen")
(setq frame-background-mode 'dark)

;; frame transparency
(set-frame-parameter (selected-frame) 'alpha '(95 50))

;; whitespace-mode
(when (require 'whitespace nil t)
  (setq whitespace-style '(face tabs tab-mark spaces space-mark))
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])))
  (setq whitespace-space-regexp "\\( +\\|\u3000+\\)")
  ;; face settings
  (set-face-foreground 'whitespace-tab "#262626")
  (set-face-background 'whitespace-tab 'nil)
  (set-face-underline 'whitespace-tab t)
  (set-face-foreground 'whitespace-space "#585858")
  (set-face-background 'whitespace-space 'nil)

  (set-face-bold-p 'whitespace-space t)
  (global-whitespace-mode 1)
  (global-set-key (kbd "C-x w") 'global-whitespace-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

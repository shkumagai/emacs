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
      '((width . 80)
        (height . 50)
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
(set-frame-parameter (selected-frame) 'alpha '(85 50))
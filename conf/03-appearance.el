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
      '((width . 200)
        (height . 58)
        (top . 10)
        (left . 0)
        ))

;; Show absolute path on title bar
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))
;; Parenthesis
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-parem-styel 'parenthesis)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; Change background color in region
(set-face-background 'region "darkgreen")
(setq frame-background-mode 'dark)

;; Frame transparency
(set-frame-parameter (selected-frame) 'alpha '(95 80))

;; Whitespace mode
(when (require 'whitespace nil t)
  (setq whitespace-style '(face tabs tab-mark spaces space-mark))
  (setq whitespace-display-mappings
        '((space-mark ?\u3000 [?\u25a1])))
  (setq whitespace-space-regexp "\\( +\\|\u3000+\\)")
  ;; Face settings
  (set-face-foreground 'whitespace-tab "#262626")
  (set-face-background 'whitespace-tab 'nil)
  (set-face-underline 'whitespace-tab t)
  (set-face-foreground 'whitespace-space "#585858")
  (set-face-background 'whitespace-space 'nil)

  (set-face-bold-p 'whitespace-space t)
  (global-whitespace-mode 1)
  (global-set-key (kbd "C-x w") 'global-whitespace-mode))

;; Remove trailing whitespace on save file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Toggle fullscreen mode
(define-key global-map (kbd "M-RET") 'ns-toggle-fullscreen)

;; Tab stops
(defun gen-tab-stop (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
         (tab-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq tab-stop-list (gen-tab-stop))

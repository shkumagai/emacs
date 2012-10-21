;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; global
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; lisp-mode-hook
(defun lisp-mode-hooks ()
  "lisp-mode-hooks"
  (require 'eldoc)
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p t)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'lisp-mode-hooks)
(add-hook 'lisp-interaction-mode-hook 'lisp-mode-hooks)
(add-hook 'ielm-mode-hook 'lisp-mode-hooks)

;; smartchr: sycle snippet
(when (require 'smartchr nil t)
  (defun cperl-mode-hooks ()
    (define-key cperl-mode-map (kbd "=") (smartchr '("=" " = " " == " " => "))))
  (add-hook 'cperl-mode-hook 'cperl-mode-hooks)
  (defun css-mode-hooks ()
    (define-key cssm-mode-map (kbd ":") (smartchr '(": " ":"))))
  (add-hook 'css-mode-hook 'css-mode-hooks))

;; wdired: enable file name edit directly in wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; insert timestamp
(defvar current-date-time-format "%c"
  "Format of date to insert `insert-current-date-time' function
See help of `format-time-string' for possible replacement")

(defvar current-time-format "%a %T"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  ;; (insert "==========\n")
  (insert (format-time-string current-date-time-format (current-time)))
  )

(defun insert-current-time ()
  "insert current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))

(global-set-key (kbd "C-c D") 'insert-current-date-time)
(global-set-key (kbd "C-c T") 'insert-current-time)

;; create new temporary buffer named "*temp*".
;; reference url:
;; - http://d.hatena.ne.jp/noqisofon/20101102/1288647885
(defun create-temporary-buffer ()
  "create and show new temporary buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*temp*"))
  (setq buffer-offer-save nil))
(global-set-key (kbd "C-c t") 'create-temporary-buffer)

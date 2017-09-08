;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; custom keybindings
(define-key global-map (kbd "C-x C-h") 'help)
(define-key global-map (kbd "C-h") 'describe-bindings)

;; insert timestamp
(setq system-time-locale "C")
(defvar current-date-time-format "%b %d, %Y %a %T"
  "Format of date to insert `insert-current-date-time' function.
See help of `format-time-string' for possible replacement")

(defvar current-time-format "%T"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "Insert current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  ;; (insert "==========\n")
  (insert (format-time-string current-date-time-format (current-time)))
  )

(defun insert-current-time ()
  "Insert current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))

(global-set-key (kbd "C-c D") 'insert-current-date-time)
(global-set-key (kbd "C-c T") 'insert-current-time)

;; create new temporary buffer named "*temp*".
;; reference url:
;; - http://d.hatena.ne.jp/noqisofon/20101102/1288647885
(defun create-temporary-buffer ()
  "Create and show new temporary buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*temp*"))
  (setq buffer-offer-save nil))
(global-set-key (kbd "C-c C-c t") 'create-temporary-buffer)

;; check dropbox
(defvar my:check-dropbox (file-exists-p (concat (getenv "HOME") "/Dropbox")))
(if my:check-dropbox (defvar my:dropbox (concat (getenv "HOME") "/Dropbox/")))

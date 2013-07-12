;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; custom keybindings
(define-key global-map (kbd "C-x C-h") 'help)
(define-key global-map (kbd "C-h") 'describe-bindings)

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
(global-set-key (kbd "C-c C-c t") 'create-temporary-buffer)


;; point-undo: undo a cursor position
;; M-x install-elisp http://www.emacswiki.org/cgi-bin/wiki/download/point-undo.el
(when (require 'point-undo nil t)
  (define-key global-map (kbd "M-[") 'point-undo)
  (define-key global-map (kbd "M-]") 'point-redo))

;; undo-tree: visualize undo branches
;; M-x package-install undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; undohist: enable undo on closed buffer
;; download from: http://cx4a.org/pub/undohist.el
(when (require 'undohist nil t)
  (undohist-initialize))


;; wdired: enable file name edit directly in wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)


;; smartchr: cycle snippet
;; emacs-smartchr: https://raw.github.com/imakado/emacs-smartchr/master/smartchr.el
(when (require 'smartchr nil t)
  ;; define hooks
  (defun my-smartchr-hooks ()
    ;; (local-set-key (kbd "=") (smartchr '("=" " = " " == " " := ")))
    ;; (local-set-key (kbd "+") (smartchr '("+" " + " " += " " += ")))
    ;; (local-set-key (kbd "-") (smartchr '("-" " - " " -= " " -= ")))

    (local-set-key (kbd ":") (smartchr '(":" ": ")))
    (local-set-key (kbd ",") (smartchr '("," ", ")))

    ;; (local-set-key (kbd ">") (smartchr '(">" " > " " >= " ">>" "->")))
    ;; (local-set-key (kbd "<") (smartchr '("<" " < " " <= " "<<" )))

    ;; (local-set-key (kbd "\"") (smartchr '("\"" "\"`!!'\"" "\"\"\"`!!'\"\"\"")))
    ;; (local-set-key (kbd "'") (smartchr '("'" "'`!!''")))

    (local-set-key (kbd "(") (smartchr '("(" "(`!!')")))
    (local-set-key (kbd "{") (smartchr '("{" "{`!!'}")))
    (local-set-key (kbd "[") (smartchr '("[" "[`!!']"))))

  ;; add to each language mode
  (add-hook 'python-mode-hook 'my-smartchr-hooks)
  (add-hook 'erlang-mode-hook 'my-smartchr-hooks)
  (add-hook 'go-mode-hook 'my-smartchr-hooks)
  )

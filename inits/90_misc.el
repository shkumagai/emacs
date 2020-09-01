;;; 90_misc.el --- miscellaneous settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


(setq system-time-locale "C")


;; Insert timestamp
(defvar current-date-time-format "%b %d, %Y %a %T"
  "Format of date to insert `insert-current-date-time' function.
See help of `format-time-string' for possible replacement")


(defvar current-time-format "%T"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")


;; Insert current date and time
(defun insert-current-date-time ()
  "Insert current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  ;; (insert "==========\n")
  (insert (format-time-string current-date-time-format (current-time))))


;; Insert current time
(defun insert-current-time ()
  "Insert current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))


;; Add key bindings
(global-set-key (kbd "C-c D") 'insert-current-date-time)
(global-set-key (kbd "C-c T") 'insert-current-time)


;; Create new temporary buffer named "*temp*"
;; reference url:
;; - http://d.hatena.ne.jp/noqisofon/20101102/1288647885
(defun create-temporary-buffer ()
  "Create and show new temporary buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*temp*"))
  (setq buffer-offer-save nil))

(global-set-key (kbd "C-c C-c t") 'create-temporary-buffer)


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 90_misc.el ends here

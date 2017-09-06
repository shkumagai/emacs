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

;; undo-tree: visualize undo branches
;; M-x package-install undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; undohist: enable undo on closed buffer
;; download from: http://cx4a.org/pub/undohist.el
(use-package undohist
  :config
  (undohist-initialize))


;; ;; wdired: enable file name edit directly in wdired
;; (use-package wdired
;;   :bind (:map dired-mode-map
;;               ("r" . wdired-change-to-wdired-mode))
;;   )
(use-package popwin
  :config
  (setq display-buffer-function 'popwin:display-buffer))

(use-package direx
  :config
  (setq direx:leaf-icon "  "
        direx:open-icon "- "
        direx:closed-icon "+ ")
  (push '(direx:direx-mode :position left :width 40 :dedicated t)
        popwin:special-display-config))
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

;; check dropbox
(defvar my:check-dropbox (file-exists-p (concat (getenv "HOME") "/Dropbox")))

(if my:check-dropbox (defvar my:dropbox (concat (getenv "HOME") "/Dropbox/")))

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; anzu
(global-anzu-mode +1)
(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threashold 1000)
 '(anzu-use-migemo t))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/skip-all-like-this))
  )

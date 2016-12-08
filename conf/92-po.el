;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; How to setting up on Mac OSX
;; ----------------------------
;;
;; Install po-mode.el extract from gettext archive.
;;
;; Refer to: http://ja.nishimotz.com/emacs_po_mode

;; (autoload 'po-mode "po-mode"
;;   "Major mode for translators to edit PO files" t)
;; (setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode)
;;                             auto-mode-alist))
(use-package po-mode
  :config
  (setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
  )

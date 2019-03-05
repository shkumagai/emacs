;;; 02-languages.el --- -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:

;;; Code:

;; Languages
;; global
(define-coding-system-alias 'UTF-8 'utf-8-unix)
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;; each window-system
(when (eq window-system 'ns)
  ;; MacOSX
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

(when (eq window-system 'w32)
  ;; Windows
  (setq default-input-method "W32-IME")

  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (w32-ime-initialize)
  (global-set-key [M-kanji] 'ignore)

  (setq file-name-coding-system 'cp932)
  (setq keyboard-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

(when (eq window-system 'x)
  ;; Linux, etc
  (use-package mozc)
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'echo-area)

  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))

;;; 02-languages.el ends here

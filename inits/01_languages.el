;;; 01_languages.el --- langunages settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; Default encodings
(set-language-environment "Japanese")
(set-locale-environment "en_US.UTF-8")

(define-coding-system-alias 'UTF-8 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(setq buffer-file-coding-system 'utf-8)
(setq save-buffer-coding-system 'utf-8-unix)
(setq process-coding-system-alist
      (cons '("grep" utf-8 . utf-8) process-coding-system-alist))


;; MacOSX
(when (eq window-system 'ns)
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;; Windows
(when (eq window-system 'w32)
  (defvar w32-ime-mode-line-state-indicator-list nil)
  (setq default-input-method "W32-IME")

  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (w32-ime-initialize)
  (global-set-key [M-kanji] 'ignore)

  (setq file-name-coding-system 'cp932)
  (setq keyboard-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;; Linux and Other XWindow System (ex. xBSD)
(when (eq window-system 'x)
  (defvar mozc-candidate-style nil)
  (leaf mozc
    :ensure t
    :require t)
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'echo-area)

  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 01_languages.el ends here

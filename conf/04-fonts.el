;;; 04-fonts.el --- -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:
;;; Code:

;; Font Settings
(when (eq window-system 'ns)
  (let* ((size 12)
         (h (* size 10))
         (asciifont "Menlo")
         (jpfont "Hiragino Maru Gothic ProN")
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; 半角カナ
    (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
    )

  ;; define aspect ratio
  (dolist (elt '((".*Hiragino Maru Gothic ProN.*" . 1.2)
                 ))
    (add-to-list 'face-font-rescale-alist elt)))

;; Windows
(when (eq window-system 'w32)
  (let* ((size 11)
         (h (* size 10))
         (asciifont "Consolas")
         (jpfont "MeiryoKe_Console")
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; 半角カナ
    (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
    )
  ;; define aspect ratio
  (dolist (elt '((".*Consolas.*" . 1.0)
                 (".*MeiryoKe_Console.*" . 1.0)
                 ))
    (add-to-list 'face-font-rescale-alist elt)))

;; Linux
(when (eq window-system 'x)
  (let* ((size 10)
         (h (* size 10))
         (asciifont "MigMix1M")
         (jpfont "Ricty")
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; 半角カナ
    (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
    )

  ;; define aspect ratio
  (dolist (elt '((".*MigMix1M.*" . 1.0)
                 (".*Ricty.*" . 1.1)
                 ))
    (add-to-list 'face-font-rescale-alist elt)))

;;; 04-fonts.el ends here

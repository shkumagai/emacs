;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Font Settings

(when (eq window-system 'ns)
  ;; set default font
  (let* ((size 14) ; ASCII font size
         (asciifont "Menlo") ; ASCII font
         ;; (jpfont "Hiragino Maru Gothic ProN") ; Japanese font
         (jpfont "Ricty")
         ;; (jpfont "Moon font")
         ;; (jpfont "Sea font")
         (h (* size 10))
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
  (dolist (elt '(("^-apple-hiragino.*" . 1.2)
                 (".*Ricty.*" . 1.2)
                 (".*Moon_font.*" . 1.2)
                 (".*Sea_font.*" . 1.2)
                 ))
    (add-to-list 'face-font-rescale-alist elt)))

;; Windows
(when (eq window-system 'w32)
  (let* ((size 11)
         (asciifont "Consolas")
         (jpfont "Takao Gothic")
         (h (* size 10))
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
                 (".*Takao Gothic.*" . 1.2)
                 ))
    (add-to-list 'face-font-rescale-alist elt)))

;; Linux
(when (eq window-system 'x)
  (let* ((size 10)
         (asciifont "Inconsolata")
         (jpfont "Ricty")
         (h (* size 10))
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
  (dolist (elt '((".*Inconsolata.*" . 1.0)
                 (".*Ricty.*" . 1.0)
                 ))
    (add-to-list 'face-font-rescale-alist elt)))

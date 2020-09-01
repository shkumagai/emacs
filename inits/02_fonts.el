;;; 02_fonts.el --- Font settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; Fonts
;; for MacOS
(when (eq window-system 'ns)
  (let* ((size 12)
         (h (* size 10))
         (asciifont "Noto Sans Mono CJK JP")
         (jpfont "Noto Sans Mono CJK JP")
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; 半角カナ
    (set-fontset-font nil '(#x0080 . #x024f) fontspec) ; 分音符付きラテン
    (set-fontset-font nil '(#x0370 . #x03ff) fontspec)) ; ギリシャ文字
  (dolist (elt '((".*Noto Sans Mono CJK JP.*" . 1.0)))
    (add-to-list 'face-font-rescale-alist elt)))

;; for Windows
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
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec)) ; ギリシャ文字
  ;; define aspect ratio
  (dolist (elt '((".*Consolas.*" . 1.0)
                 (".*MeiryoKe_Console.*" . 1.0)))
    (add-to-list 'face-font-rescale-alist elt)))

;; for Linux
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
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec)) ; ギリシャ文字
  ;; define aspect ratio
  (dolist (elt '((".*MigMix1M.*" . 1.0)
                 (".*Ricty.*" . 1.1)))
    (add-to-list 'face-font-rescale-alist elt)))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 02_fonts.el ends here

;; Font Settings

(when (eq window-system 'ns)
  ;; set default font
  (let* ((size 14) ; ASCII font size
         (asciifont "Menlo") ; ASCII font
         ;; (jpfont "Hiragino Maru Gothic ProN") ; Japanese font
         ;; (jpfont "Ricty")
         (jpfont "Moon font")
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

  ;; フォントサイズの比を設定
  (dolist (elt '(("^-apple-hiragino.*" . 1.2)
                 (".*Ricty.*" . 1.2)
                 (".*Moon_font.*" . 1.2)
                 (".*Sea_font.*" . 1.2)
                 ))
    (add-to-list 'face-font-rescale-alist elt)))

;; Windows
(when (eq window-system 'w32)
  (set-face-attribute 'default nil
          :family "Consolas"
          :height 110)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo")))

;; Linux
(when (eq window-system 'x)
  (set-face-attribute 'default nil
         :family "Liberation Mono"
         :height 100)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Takao Gothic"))
  (setq face-font-rescale-alist
 '((".*Liberation Mono.*" . 1.0)
   (".*Takao Gothic.*" . 1.2)
   )))

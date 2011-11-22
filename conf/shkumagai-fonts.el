;; Font Settings

(when (eq window-system 'ns)
  ;; set default font
  (set-face-attribute 'default nil
                      :family "Ricty"
                      ;; 14pt
                      :height 180)
  ;; Whole Japanese Characters
  ;; (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Hiragino_Kaku_Gothic_ProN"))
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Moon_font"))
  ;; (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Sea_font"))
  ;; (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))
  ;; Only 'かな' and 'カナ'
  ;; U+3000-303F CJKの記号および句読点
  ;; U+3040-309F ひらがな
  ;; U+30A0-30FF カタカナ
  (set-fontset-font nil '( #x3040 . #x30ff) (font-spec :family "Moon_font"))
  ;; (set-fontset-font nil '( #x3040 . #x30ff) (font-spec :family "Sea_font"))
  ;; (set-fontset-font nil '( #x3040 . #x30ff) (font-spec :family "Ricty"))

  ;; Aspect Ratio
  (setq face-font-rescale-alist
 '((".*Ricty.*" . 1.0)
   ;; (".*Hiragino_Kaku_Gothic_ProN.*" . 1.0)
   (".*Moon_font.*" . 1.0)
   ;; (".*Sea_font.*" . 1.0)
   )))

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

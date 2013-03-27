;; text-translator
(when (require 'text-translator nil t)
  (setq text-translator-auto-selection-func
        'text-translator-translate-by-auto-selection-enja)

  (global-set-key (kbd "C-x M-t") 'text-translator)
  (global-set-key (kbd "C-x M-T") 'text-translator-translate-last-string)
  )

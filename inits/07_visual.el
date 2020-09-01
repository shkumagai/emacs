;;; 07_visual.el --- Visual settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; Parenthesis
(leaf paren
  :ensure nil
  :commands show-paren-mode
  :hook ((after-init-hook . show-paren-mode))
  :custom ((show-paren-style quote mixed)
           (show-paren-when-point-inside-paren . t)
           (show-paren-when-point-in-periphery . t))
  :custom-face ((show-paren-mode quote
                                 ((nil
                                   (:background "#44475a" :foreground "#f1fa8c"))))))


;; Indent highlighting
;; https://github.com/DarthFennec/highlight-indent-guides
(leaf highlight-indent-guides
  :hook (prog-mode-hook)
  :custom ((highlight-indent-guides-auto-enable . t)
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-character . 124)
           (highlight-indent-guides-method quote character))
  :config
  (with-eval-after-load 'highlight-indent-guides
    (if (fboundp 'diminish)
        (diminish 'highlight-indent-guides-mode))))  ; column


;; Whitespace mode
(leaf whitespace
  :ensure t
  :bind (("C-x w" . global-whitespace-mode))
  :custom ((whitespace-style . '(face trailing tabs spaces empty tab-mark space-mark))
           (whitespace-display-mappings . '((space-mark 12288 [9633])
                                            (tab-mark 9 [187 9] [92 9])))
           (whitespace-space-regexp . "\\( +\\|　+\\)"))
  :config
  (with-eval-after-load 'whitespace
    (global-whitespace-mode 1)
    (defvar my/bg-color "#2d3743")
    (set-face-attribute 'whitespace-trailing nil :background my/bg-color :foreground "DeepPink" :underline t)
    (set-face-attribute 'whitespace-tab nil :background my/bg-color :foreground "LightSkyBlue" :underline t)
    (set-face-attribute 'whitespace-space nil :background my/bg-color :foreground "GreenYellow" :weight 'bold)
    (set-face-attribute 'whitespace-empty nil :background my/bg-color)))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 07_visual.el ends here

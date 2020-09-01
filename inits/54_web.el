;;; 54_web.el --- Web mode settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; HTML and Psedo HTML (for template language files)
(leaf web-mode
  :ensure t
  :preface
  (defun web-mode-hook nil
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 4))

  :mode ("\\.phtml$" "\\.tpl\\.php$" "\\.jsp$" "\\.as[cp]x$" "\\.erb$" "\\.html?$")
  :hook ((web-mode-hook . web-mode-hook))
  :require t
  :config
  (when (< emacs-major-version 24)
    (defalias 'prog-mode 'fundamental-mode)))


;; CSS
(leaf css-mode
  :ensure t
  :mode ("\\.css\\(_t\\)\\'")
  :require t)


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 54_web.el ends here

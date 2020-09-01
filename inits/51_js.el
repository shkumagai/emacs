;;; 51_js.el --- JavaScript/TypeScript mode settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; JavaScript
;; js2-mode
(leaf js2-mode
  :ensure t
  :mode ("\\.js\\(_t\\)\\'")
  :require t)


;; React jsx-mode
(leaf rjsx-mode
  :ensure t
  :mode ("\\.jsx\\'")
  :require t)


;; TypeScript
;; https://github.com/emacs-typescript/typescript.el
(leaf typescript-mode
  :ensure t
  :mode ("\\.ts\\'")
  :require t)


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 51_js.el ends here

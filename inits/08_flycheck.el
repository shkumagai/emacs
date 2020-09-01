;;; 08_flycheck.el --- FlyCheck settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; Flycheck: A modern on-the-fly syntax checking extension, and a modern alternative to Flymake.
;; https://www.flycheck.org/en/latest/
(leaf flycheck
  :ensure t
  :hook ((after-init-hook . global-flycheck-mode))
  :require t)


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 08_flycheck.el ends here

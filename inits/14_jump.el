;;; 14_jump.el --- Jump settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;;;; dumb-jump: jump to definition for multiple languages without configuration.
;; https://github.com/jacktasia/dumb-jump
(leaf dumb-jump
  :ensure t
  :bind (([(super d)]       . dumb-jump-go)
         ([(super shift d)] . dumb-jump-back))
  :require t
  :setq ((dumb-jump-mode . t)
         (dumb-jump-selector quote ivy)
         (dumb-jump-use-visible-window)))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 14_jump.el ends here

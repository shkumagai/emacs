;;; 04_cursor.el --- Cursor settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; Anzu: an Emacs port of anzu.vim
;; https://github.com/syohex/emacs-anzu
(leaf anzu
  :ensure t
  :require t
  :config
  (global-anzu-mode 1))


;; Multiple-Cursors
;; https://github.com/magnars/multiple-cursors.el
(leaf multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/skip-all-like-this)))


;; Beacon
;; https://github.com/Malabarba/beacon
(leaf beacon
  :custom ((beacon-size . 60)
           (beacon-color . "yellow")
           (beacon-blink-delay . 0.5)
           (beacon-blink-dulation . 0.5))
  :require t
  :config
  (beacon-mode 1))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 04_cursor.el ends here

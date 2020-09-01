;;; 58_po.el --- PO mode settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; PO file (from gettext)
(leaf po-mode
  :config
  (setq auto-mode-alist (cons
                         '("\\.po\\'\\|\\.po\\." . po-mode)
                         auto-mode-alist)))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 58_po.el ends here

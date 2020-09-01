;;; 10_.git.el --- Git settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; Magit: an interface to the version control system Git.
;; https://github.com/magit/magit
(leaf git
  :ensure t
  :bind (("C-c C-g" . magit-status)))


;; git-gutter
(leaf git-gutter
  :ensure t
  :custom ((git-gutter:modified-sign . "~")
           (git-gutter:added-sign    . "+")
           (git-gutter:deleted-sign  . "-"))
  :custom-face ((git-gutter:modified . '((t (:background "#f1fa8c"))))
                (git-gutter:added    . '((t (:background "#50fa7b"))))
                (git-gutter:deleted  . '((t (:background "#ff79c6")))))
  :require t
  :config
  (global-git-gutter-mode 1))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 10_git.el ends here

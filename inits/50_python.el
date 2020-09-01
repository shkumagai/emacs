;;; 50_python.el --- Python mode settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(add-hook 'python-mode-hook #'lsp)


;; ;; jedi
;; ;; https://github.com/tkf/emacs-jedi
;; (use-package jedi-core
;;   :config
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:use-shortcuts t)
;;   (defun my/python-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'my/python-mode-hook))


;; virtualenvwrapper
;; https://github.com/porterjamesj/virtualenvwrapper.el
(leaf virtualenvwrapper
  :ensure t
  :require t
  :setq ((venv-location . "~/.virtualenvs")))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 50_python.el ends here

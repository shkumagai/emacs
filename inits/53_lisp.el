;;; 53_lisp.el --- CL mode settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; lisp-mode-hook
(defun lisp-mode-hooks ()
  "'lisp-mode-hooks'."
  (leaf eldoc
    :ensure t
    :require t
    :setq ((eldoc-idle-delay . 0.2)
           (eldoc-echo-area-use-multiline-p . t))
    :config
    (turn-on-eldoc-mode)))

(add-hook 'emacs-lisp-mode-hook 'lisp-mode-hooks)
(add-hook 'lisp-interaction-mode-hook 'lisp-mode-hooks)
(add-hook 'ielm-mode-hook 'lisp-mode-hooks)


;; Common Lisp
;; Setup load-path, autoloads and your Lisp system.
;; (add-to-list 'load-path "~/.emacs.d/elisp/slime")
;; (use-package slime-autoloads
;;   :config
;;   (setq inferior-lisp-program "sbcl")
;;   (setq slime-net-coding-system 'utf-8-unix)
;;   (slime-setup '(slime-repl slime-fancy slime-banner slime-indentation slime-company))
;;   )


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 53_lisp.el ends here

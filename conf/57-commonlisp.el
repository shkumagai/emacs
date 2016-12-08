;; Setup load-path, autoloads and your lisp system.

;; (add-to-list 'load-path "~/.emacs.d/elisp/slime")
(use-package slime-autoloads
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-repl slime-fancy slime-banner slime-indentation slime-company))
  )

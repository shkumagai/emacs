;; Setup load-path, autoloads and your lisp system.

;; (add-to-list 'load-path "~/.emacs.d/elisp/slime")
(when (require 'slime-autoloads nil t)
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-repl)))

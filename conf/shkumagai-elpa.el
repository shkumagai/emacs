;; elap (package.el)
(when (require 'package nil t)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (setq package-user-dir (concat user-emacs-directory "vendor/elpa"))
  (package-initialize))

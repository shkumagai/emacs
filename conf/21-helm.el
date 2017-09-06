;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

(use-package helm-config
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("C-;" . helm-mini)
   :map helm-find-files-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)
   )
  :config
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  )

;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Exec Paths
(unless (eq window-system 'w32)
  (when (require 'exec-path-from-shell nil t)
    (exec-path-from-shell-initialize))
    ;; (let ((envs '("PATH" "GOPATH")))
    ;;   (exec-path-from-shell-copy-envs envs)))
  )

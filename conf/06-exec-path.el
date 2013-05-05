;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Exec Paths
(when (eq window-system 'ns)
  (add-to-list 'exec-path "/opt/local/sbin")
  (add-to-list 'exec-path "/opt/local/bin")
  )

(add-to-list 'exec-path "/usr/local/sbin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/bin")

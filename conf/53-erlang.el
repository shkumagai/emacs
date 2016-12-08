;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Erlang mode
;;
;; $ find /usr/lib /usr/lib64 /opt/local/lib -type d -name "erlang"
(when (eq window-system 'ns)
  (setq erlang-root-dir "/opt/local/lib/erlang"))

(when (eq window-system 'x)
  (setq erlang-root-dir "/usr/lib64/erlang"))

(defun get-emacs-ext-dir ()
  (interactive)
  (replace-regexp-in-string "\n+$"
                            ""
                            (shell-command-to-string
                             (format "find %s -name \"emacs\" -type d -print"
                                     erlang-root-dir))))

;; $ find /usr/lib /usr/lib64 /opt/local/lib -type d -name "emacs"
(add-to-list 'load-path (get-emacs-ext-dir))

(add-to-list 'exec-path (concat erlang-root-dir "/bin"))
(use-package erlang-start)

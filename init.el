;;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; @shkumagai Emacs Environemt
;; git://github.com/shkumagai/emacs.git

;; Library Paths
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name
                                (concat user-emacs-directory path))))
    (add-to-list 'load-path default-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))))

;; add-to-load-path
(add-to-load-path "conf" "elisp")

;; require Common-Lisp functions.
(require 'cl)

;; basic settings
(load-library "01-elpa")            ; Package Management
(load-library "02-languages")       ; Language Settings
(load-library "03-appearance")      ; Appearance
(load-library "04-fonts")           ; Fonts
(load-library "05-color-theme")     ; Color Theme
(load-library "06-exec-path")       ; Exec Path

(load-library "11-lisp-mode-hooks") ; lisp-mode-hook setting
(load-library "12-auto-install")    ; auto-install.el (only for Anything)
(load-library "19-misc")            ; miscellaneous settings

;; additional settings
(load-library "21-anything")        ; Anything
(load-library "22-auto-complete")   ; auto-complete
;; (load-library "23-yasnippet")       ; YASnippet
(load-library "24-git")             ; Git on Emacs (using magit)
(load-library "25-migemo")          ; Migemo
(load-library "26-ctags")           ; Ctags
(load-library "27-tabbar")          ; Tabbar
(load-library "28-po")              ; PO

;; for programming languages
(load-library "51-perl")            ; perl
(load-library "52-python")          ; python, rst
;; (load-library "53-erlang")          ; erlang
;; (load-library "54-go")              ; go
(load-library "55-css")             ; css
(load-library "56-js")              ; js

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((syntax . elisp)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
)

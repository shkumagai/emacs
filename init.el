;;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; @shkumagai Emacs Environemt
;; git://github.com/shkumagai/emacs.git

;; Library Paths

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

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
(load-library "03-exec-path")       ; Exec Path
(load-library "04-fonts")           ; Fonts
(load-library "06-appearance")      ; Appearance
(load-library "05-color-theme")     ; Color Theme

(load-library "11-lisp-mode-hooks") ; lisp-mode-hook setting
(load-library "12-auto-install")    ; auto-install.el (only for Anything)
(load-library "19-misc")            ; miscellaneous settings

;; additional settings
(load-library "21-anything")        ; Anything
(load-library "22-ctags")           ; Ctags
(load-library "23-migemo")          ; Migemo
(load-library "24-tabbar")          ; Tabbar
(load-library "25-git")             ; Git on Emacs (using magit)
(load-library "28-yasnippet")       ; YASnippet
(load-library "29-company")         ; company

;; for programming languages
(load-library "51-perl")            ; perl
(load-library "52-python")          ; python (jedi)
(load-library "53-erlang")          ; erlang
(load-library "54-go")              ; go
(load-library "55-groovy")          ; groovy
(load-library "56-coffee")          ; coffee script
(load-library "56-js")              ; js
(load-library "57-commonlisp")      ; common-lisp (SLIME)

;; for markup languages
(load-library "71-web-mode")        ; web-mode
(load-library "72-css")             ; css
(load-library "73-yaml")            ; YAML

;; for domain specified format
(load-library "91-ansible")         ; Ansible
(load-library "92-po")              ; PO

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threashold 1000)
 '(anzu-use-migemo t)
 '(safe-local-variable-values (quote ((syntax . elisp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

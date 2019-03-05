;;; init.el --- My Emacs configuration file  -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;;; Commentary:

;; Filename: init.el
;; Description: My Emacs configuration file
;; Package-Requires: ((Emacs "26.1"))
;; Author: KUMAGAI, Shoji <take.this.t.your.grave_at_gmail.com>
;; Created: 2011-11-17
;; Modified: 2019-03-05
;; URL: https://github.com/shkumagai/emacs/init.el

;;; Code:

(defun add-to-load-path (&rest paths)
  "Add specified PATHS to 'load-path'."
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
(eval-when-compile (require 'cl))

;; basic settings
(load-library "01-elpa")            ; Package Management
(load-library "02-languages")       ; Language Settings
(load-library "03-exec-path")       ; Exec Path
(load-library "04-fonts")           ; Fonts
(load-library "05-faces-builtin")   ; Faces(Appearance) with builtin commands
(load-library "06-faces-packages")  ; Faces(Appearance) with external packages
(load-library "07-color-theme")     ; Color Theme
(load-library "09-misc")            ; Miscellneous settings

(load-library "11-lisp-mode-hooks") ; lisp-mode-hook setting
(load-library "13-history")         ; Undohist, HistTree
(load-library "14-multiplecursors") ; multiple-cursors
(load-library "15-flycheck")        ; flycheck
(load-library "16-anzu")            ; anzu

;; additional settings with packages
(load-library "21-counsel")         ; Counsel
(load-library "22-company")         ; company
(load-library "23-migemo")          ; Migemo
(load-library "24-neotree")         ; nettree
(load-library "25-git")             ; Git on Emacs (using magit)

;; for programming languages
(load-library "51-perl")            ; perl
(load-library "52-python")          ; python (jedi)
(load-library "53-js")              ; javascript
(load-library "54-ts")              ; typescript
;; (load-library "57-commonlisp")      ; common-lisp (SLIME)

;; for markup languages
(load-library "71-web-mode")        ; web-mode
(load-library "72-css")             ; CSS
(load-library "73-yaml")            ; YAML
(load-library "74-markdown")        ; Markdown
(load-library "75-rest")            ; reStructuredText

;; for domain specified format
(load-library "91-po")              ; PO file

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(max-specpdl-size 20000)
 '(max-lisp-eval-depth 20000)
 ;; '(anzu-deactivate-region t)
 ;; '(anzu-mode-lighter "")
 ;; '(anzu-search-threashold 1000)
 ;; '(anzu-use-migemo t)
 '(safe-local-variable-values (quote ((syntax . elisp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here

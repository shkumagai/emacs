;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Go

;; need to install godef
;; % go get -u code.google.com/p/rog-go/exp/cmd/godef
(when (require 'go-mode-load nil t)
  (add-hook 'before-save-hook 'gofmt-before-save))

;; need to install gocode
;; % go get -u github.com/nsf/gocode
(when (require 'go-autocomplete nil t))

(when (require 'go-eldoc nil t)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t :foreground "green"
                      :weight 'bold))

;; need to install gotags
;; % go get -u github.com/jstemmer/gotags
(when (require 'go-direx nil t)
  (define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer))

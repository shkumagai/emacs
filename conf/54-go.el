;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Go
(use-package go-mode
  :config
  ;; need to install godef
  ;; % go get -u github.com/rogpeppe/godef
  ;; % go get -u github.com/nsf/gocode
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-i") 'go-goto-imports)))
  (add-hook 'go-mode-hook '(lambda ()
                             (local-set-key (kbd "C-c C-f") 'gofmt)))
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook '(lambda ()
                             (set (make-local-variable 'company-backends) '(company-go))
                             (company-mode))))

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t :foreground "green"
                      :weight 'bold))

(use-package go-direx
  ;; need to install gotags
  ;; % go get -u github.com/jstemmer/gotags
  :bind (:map go-mode-map
              ("C-c C-j" . go-direx-pop-to-buffer))
  )

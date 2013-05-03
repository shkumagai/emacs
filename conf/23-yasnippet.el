;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; How to setting up on MacOSX

;; Install YASnippet package in your own environment according to installation
;; direction in the document.
;;
;; Project site: http://capitaomorte.github.com/yasnippet/
;;
;; And then, add elisp snippet as below to your .emacs or init.el.
;;
;; Currently, yasnippets.el is able to install with package.el.

;; Yet another snippet
(require 'yasnippet)
(setq yas-installed-dir (installed-dir "yasnippet"))
(setq yas-snippet-dirs
      (let (dirs)
        (dolist (d '(yas-installed-dir))
          (setq dirs (cons (concat (symbol-value d) "/snippets") dirs)))
        dirs)
      )
(yas-global-mode 1)

;; Yet another snippet configuration
;; refer to: http://fukuyama.co/yasnippet

;; Insert default snippet
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)

;; Create buffer to edit new snippet
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)

;; View & Edit all snippets
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

;; Yet another snippet anything interface
;; refer to: https://github.com/sugyan/dotfiles/blob/master/.emacs.d/conf/04-yasnippet.el
(eval-after-load "anything-config"
  '(progn
     (defun my-yas/prompt (prompt choices &optional display-fn)
       (let* ((names (loop for choice in choices
                           collect (or (and display-fn (funcall display-fn choice))
                                       choice)))
              (selected (anything-other-buffer
                         `(((name . ,(format "%s" prompt))
                            (candidates . names)
                            (action . (("Insert snippet" . (lambda (arg) arg))))))
                         "*anything yas/prompt*")))
         (if selected
             (let ((n (position selected names :test 'equal)))
               (nth n choices))
           (signal 'quit "user quit!"))))
     (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
     (define-key anything-command-map (kbd "y") 'yas/insert-snippet)))

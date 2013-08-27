;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

(when (require 'tabbar nil t)
  (tabbar-mode 1)
  (tabbar-mwheel-mode -1)
  (setq tabbar-buffer-groups-function nil)

  ;; disabling buttons
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))

  (setq tabbar-auto-scroll-flag nil)
  (setq tabbar-separator '(1.5))

  (set-face-attribute
   'tabbar-default nil
   :family "Menlo"
   :background "black"
   :foreground "gray72"
   :height 1.0)
  (set-face-attribute
   'tabbar-unselected nil
   :background "black"
   :foreground "gray72"
   :box nil)
  (set-face-attribute
   'tabbar-selected nil
   :background "black"
   :foreground "#c82829"
   :box nil)
  (set-face-attribute
   'tabbar-button nil
   :box nil)
  (set-face-attribute
   'tabbar-separator nil
   :height 1.2)

  (defvar my-tabbar-displayed-buffers
    '("*scratch*" "*Messages*" "*Backtrace*" "*Colors*" "*Faces*" "*vc-")
    "*Regexps matches buffer name always included tabs.")
  (defun my-tabbar-buffer-list ()
    "Return the list of buffers to show in tab.
  Exclude buffers whose name starts with a space or an asterisk.
  The current buffer and buffers match `my-tabbar-displayed-buffers'
  are always included."
    (let* ((hides (list ?\  ?\*))
           (re (regexp-opt my-tabbar-displayed-buffers))
           (cur-buf (current-buffer))
           (tabs (delq nil
                       (mapcar (lambda (buf)
                                 (let ((name (buffer-name buf)))
                                   (when (or (string-match re name)
                                             (not (memq (aref name 0) hides)))
                                     buf)))
                               (buffer-list)))))
      ;; Always include the current buffer
      (if (memq cur-buf tabs)
          tabs
        (cons cur-buf tabs))))
  (setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

  (global-set-key (kbd "s-{") 'tabbar-forward-tab)
  (global-set-key (kbd "s-}") 'tabbar-backward-tab)
  )

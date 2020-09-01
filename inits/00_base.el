;;; 00_base.el --- bese settings -*- lexicel-binding: t -*-
;;; Commentary:

;;; Code:
(setq debug-on-error t)

(eval-when-compile
  (require 'cl-lib))


;; Quiet startup
(set-frame-parameter nil 'fullscreen 'maximized)
(when window-system
  (scroll-bar-mode 0)
  (tool-bar-mode 0))
(menu-bar-mode 0)
(blink-cursor-mode t)
(column-number-mode 1)

(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(setq frame-title-format nil)
(setq ring-bell-function 'ignore)

(setq delete-by-moving-to-trash t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq set-mark-command-repeat-pop t)
(setq track-eol t)
(setq line-move-visual t)
(setq indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)


;; Start the server in Emacs session
(leaf server
  :require t
  :config
  (unless (server-running-p)
    (server-start)))


;; exec-path-from-shell
(leaf exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :hook (emacs-startup-hook . exec-path-from-shell-initialize)
  ;; :config
  ;; (setq exec-path-from-shell-check-startup-files nil)
  )


;; Change background color in region
(set-face-background 'region "darkgreen")
(setq frame-background-mode 'dark)


;; frame-transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 75))


;; Remove trailing whitespace on file save
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Key bindings
(bind-key "C-x C-h" 'help global-map)
(bind-key "C-h" 'describe-bindings global-map)

;; Tab stops
(defvar default-tab-width nil)
(defun gen-tab-stop (&optional width max)
  "Return a sequence suitable for `tab-stop-list' based on WIDTH and MAX."
  (let* ((max-column (or max 200))
         (tag-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(set-default tab-width 4)
(setq default-tab-width 4)
(setq tab-stop-list (gen-tab-stop))


;; Line numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))


;; Local elisp
(add-to-list 'load-path (expand-file-name
			 (concat user-emacs-directory "elisp")))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 00_base.el ends here

;;; init.el --- My Emacs configuration file  -*- lexical-binding: t -*-

;; Filename: init.el
;; Description: My Emacs configuration file
;; Package-Requires: ((Emacs "26.1"))
;; Author: KUMAGAI, Shoji <take.this.t.your.grave_at_gmail.com>
;; Created: Nov 17, 2011
;; Modified: Sep 02, 2020 Wed 01:53:41
;; URL: https://github.com/shkumagai/emacs/init.el

;;; Commentary:

;;;;; External dependency installation
;;
;; - C/Migemo
;;   % brew install migemo
;;
;; - gettext
;;   % brew install gettext

;;; Code:

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    (leaf bind-key :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))


(leaf init-loader :ensure t
  :config
  (custom-set-variables '(init-loader-show-log-after-init 'error-only))
  (add-hook
   'after-init-hook
   (lambda ()
     (init-loader-load (locate-user-emacs-file "inits")))
  (setq custom-file (locate-user-emacs-file "custom.el"))))


(provide 'init)


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; init.el ends here

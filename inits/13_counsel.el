;;; 13_counsel.el --- Counsel settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;;;; Ivy/Counsel/Swiper
;; https://github.com/abo-abo/swiper
;;   This repository contains:
;;   Ivy, a generic completion mechanism for Emacs.
;;   Counsel, a collection of Ivy-enhanced versions of common Emacs commands.
;;   Swiper, an Ivy-enhanced alternative to isearch.
(leaf counsel
  :ensure t
  :bind (("" . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("<f2> u" . counsel-unicode-char)
         ("C-c f" . counsel-recentf)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhythmbox))
  :custom ((ivy-use-virtual-buffers . t)
           (enable-recursive-minibuffers . t)
           (ivy-height . 30)
           (ivy-extra-directories)
           (ivy-re-builders-alist quote
                                  ((t . ivy--regex-plus))))
  :require t
  :config
  (ivy-mode 1)
  (counsel-mode 1))


;; ivy-rich
(leaf ivy-rich
  :ensure t
  :after ivy
  :require t
  :config
  (ivy-rich-mode 1))


;; all-the-icons-ivy
(leaf all-the-icons-ivy
  :ensure t
  :require t
  :config
  (all-the-icons-ivy-setup))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 13_counsel.el ends here

;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

(when (require 'package nil t)
  (add-to-list
   'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list
   'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (unless (file-directory-p (concat user-emacs-directory "elpa/archives"))
    (package-list-packages))
  (package-initialize))

;; install packages automatically by package.el
;; refer to: http://blog.64p.org/entry/2013/05/01/233306
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
 '(auto-complete
   ctags
   gist
   magit
   undo-tree
   yaml-mode
   yasnippet
   ))

;; delete buffer if it opened
(dolist (buf '("*Packages*" "*Compile-Log*"))
  (cond ((get-buffer buf)
         (kill-buffer buf))))

;; private function
(defun installed-dir (package)
  "Return directory path where package with specified name is installed."
  (let ((dname
        (car (loop for p in (directory-files package-user-dir)
                   when (string-match (concat package "-*") p) collect p))))
  (concat package-user-dir "/" dname)))
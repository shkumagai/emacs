;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Perl
(defalias 'perl-mode 'cperl-mode)
(setq auto-mode-alist
      (cons '("\\.t\\'" . cperl-mode) auto-mode-alist))
(setq cperl-auto-newline nil)
(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -4)
(setq cperl-indent-level 4)
(setq cperl-level-offset -4)
(setq cperl-continuted-statement-offset 4)
(setq cperl-highlight-variables-indiscriminaly t)
(add-hook 'cperl-mode-hook
      (lambda ()
	(set-face-italic-p 'cperl-hash-face nil)))
(add-hook 'cperl-mode-hook
      '(lambda ()
	 (define-key cperl-mode-map (kbd "C-c c") 'cperl-check-syntax)
	 (setq indent-tabs-mode nil)))

;; perltidy
(defmacro mark-active ()
  "xemacs/emacs compatibility macro"
  (if (boundp 'mark-active)
      'mark-active
    '(mark)))

(defun perltidy ()
  "Run perltidy on the current region or buffer."
  (interactive)
  ;; Inexplicably. save-excursion doesn't work here.
  (let ((orig-point (point)))
    (unless (mark-active) (mark-defun))
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)
    (goto-char orig-point)))

(global-set-key (kbd "C-c t") 'perltidy)

(defvar perltidy-mode nil
  "Automatically 'perltidy' when saving.")
(make-variable-buffer-local 'perltidy-mode)
(defun perltidy-write-hook ()
  "Perltidy a buffer during 'write-file-hooks' for 'perltidy-mode'."
  (if perltidy-mode
      (save-excursion
        (widen)
        (mark-whole-buffer)
        (not (perltidy)))
    nil))
(defun perltidy-mode (&optional arg)
  "Perltidy minor mode."
  (interactive "P")
  (setq perltidy-mode
        (if (null arg)
            (not perltidy-mode)
          (> (prefix-numeric-value-arg) 0)))
  (mark-local-hook 'write-file-hooks)
  (if perltidy-mode
      (add-hook 'write-file-hooks 'perltidy-write-hook)
    (remove-hook 'write-file-hooks 'perltidy-write-hook)))
(if (not (assq 'perltidy-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(perltidy-mode " Perltidy")
                minor-mode-alist)))
(eval-after-load "cperl-mode"
  '(add-hook 'cperl-mode-hook 'perltidy-mode))

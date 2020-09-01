;;; 52_perl.el --- Perl mode settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;;;; Perl
;;;;; custumizing cperl-mode
(defalias 'perl-mode 'cperl-mode)
(setq auto-mode-alist
      (cons '("\\.t$" . cperl-mode) auto-mode-alist))
(custom-set-variables '(cperl-indent-level 4)
                      '(cperl-continuted-statement-offset 4)
                      '(cperl-close-paren-offset -4)
                      '(cperl-level-offset -4)
                      '(cperl-comment-column 40)
                      '(cperl-highlight-variables-indiscriminaly t)
                      '(cperl-indent-parens-as-block t)
                      '(cperl-tab-always-indent nil)
                      '(cperl-font-lock t))
(add-hook 'cperl-mode-hook
          '(lambda ()
             (progn
               (setq indent-tabs-mode nil)
               (setq tab-width nil)
               (cperl-set-style "PerlStyle")

               ;; perl completion
               (use-package auto-completion)
               (use-package perl-completion)
               (add-to-list 'ac-source 'ac-source-perl-completion)
               (perl-completion-mode t))))

;;;;; perltidy
(defmacro mark-active ()
  "XEmacs/Emacs compatibility macro."
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
  "Perltidy minor mode with ARG."
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


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 52_perl.el ends here

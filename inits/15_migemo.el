;;; 15_migemo.el --- Migemo search settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; Migemo: Japanese increment search with 'Romanization of Japanese'(ローマ字).
;; https://github.com/emacs-jp/migemo
(leaf migemo
  :when (executable-find "cmigemo")
  :custom ((migemo-command . "cmigemo")
           (migemo-user-dictionary)
           (migemo-regex-dictionary)
           (migemo-use-pattern-alist . t)
           (migemo-use-frequent-pattern-alist . t)
           (migemo-pattern-alist-length . 1000)
           (migemo-coding-system quote utf-8-unix))
  :require t
  :config
  (cond
   ((eq system-type 'darwin)
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
   ((eq system-type 'gnu/linux)
    (setq migemo-options '("-q" "--emacs" "-i" ""))
    (setq migemo-dictionary "/usr/share/migemo/migemo-dict")))
  (migemo-init))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 15_migemo.el ends here

;;; 55_markdown.el --- Markdown mode settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; Markdown
(leaf markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         "\\.md\\'" "\\.markdown\\'")
  :setq ((markdown-command . "multimarkdown")))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 55_markdown.el ends here

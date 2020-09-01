;;; 05_history.el --- History settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; History Handling
;; undo-tree: visualize undo branches
;; https://www.emacswiki.org/emacs/UndoTree
(leaf undo-tree
  :ensure t
  :require t
  :config
  (global-undo-tree-mode))


;; undohist: enable undo on closed buffer
;; https://github.com/emacsmirror/undohist
(leaf undohist
  :ensure t
  :require t
  :config
  (undohist-initialize))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 05_history.el ends here

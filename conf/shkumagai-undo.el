;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; point-undo: undo a cursor position
(when (require 'point-undo nil t)
  (define-key global-map (kbd "M-[") 'point-undo)
  (define-key global-map (kbd "M-]") 'point-redo))

;; undo-tree: visualize undo branches
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; undohist: enable undo on closed buffer
(when (require 'undohist nil t)
  (undohist-initialize))

;; end
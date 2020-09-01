;;; 06_ui.el --- UI settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; doom-themes
(leaf doom-themes
  :ensure t
  :custom ((doom-themes-enable-bold . t)
           (doom-themes-enable-italic . t))
  :custom-face ((doom-modeline-bar quote ((t (:background "#6272a4")))))
  :require t
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))


;; doom-modeline
(leaf doom-modeline
  :ensure t
  :hook (after-init-hook)
  :custom ((doom-modeline-buffer-file-name-style quote truncate-with-project)
           (doom-modeline-icon . t)
           (doom-modeline-major-mode-icon . t)
           (doom-modeline-minor-modes))
  :config
  (with-eval-after-load 'doom-modeline
    (line-number-mode 0)
    (column-number-mode 0)
    (doom-modeline-def-modeline 'main
      '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))))


;; Neotree: A emacs tree plugin like NERD tree for Vim.
;; https://github.com/jaypei/emacs-neotree
(leaf neotree
  :ensure t
  :defvar neo-persist-show
  :bind (("<f8>" . neotree-toggle))
  :config
  (with-eval-after-load 'neotree
    (setq neo-theme (if (display-graphic-p)
                        'icons 'arrows))
    (setq neo-persist-show t)
    (setq neo-mode-line-type 'none)
    (setq neo-smart-open t)
    (setq neo-window-width 45)
    (add-hook 'neo-after-create-hook
              (lambda (&rest _)
                (display-line-numbers-mode -1)))))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 06_ui.el ends here

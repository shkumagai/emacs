;;; 11_lsp.el --- LSP settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; lsp-mode
(leaf lsp-mode
  :ensure t
  :commands lsp-prog-major-mode-enable lsp
  :hook ((prog-major-mode-hook . lsp-prog-major-mode-enable))
  :custom ((lsp-print-io)
           (lsp-trace)
           (lsp-print-performance)
           (lsp-auto-guess-root . t)
           (lsp-response-timeout . 5)
           (lsp-prefer-flymake quote flymake)))


;; lsp-ui
(leaf lsp-ui
  :ensure t
  :after lsp-mode
  :bind ((lsp-mode-map
          ("C-c C-r" . lsp-ui-peek-find-references)
          ("C-c C-j" . lsp-ui-peek-find-definitions)
          ("C-c i" . lsp-ui-peed-find-implementation)
          ("C-c m" . lsp-ui-imenu)
          ("C-c s" . lsp-ui-sideline-mode)))
  :hook (lsp-mode-hook)
  :custom ((lsp-ui-flycheck-enable)
           (lsp-ui-sideline-enable)
           (lsp-ui-sideline-ignore-duplicate . t)
           (lsp-ui-sideline-show-symbol . t)
           (lsp-ui-sideline-show-hover . t)
           (lsp-ui-sideline-show-diagnostics)
           (lsp-ui-sideline-show-code-actions)
           (lsp-ui-imenu-enable)
           (lsp-ui-imenu-kind-position quote top)
           (lsp-ui-peek-enable . t)
           (lsp-ui-peek-peek-height . 20)
           (lsp-ui-peek-list-width . 50)
           (lsp-ui-peek-fontify quote on-demand)))


;; company-lsp
(leaf company-lsp
  :ensure t
  :after company lsp-mode
  :commands company-lsp
  :custom ((company-lsp-cache-candidates . t)
           (company-lsp-async . t)
           (company-lsp-enable-recompletion)))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 11_lsp.el ends here

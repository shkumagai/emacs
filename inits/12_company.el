;;; 12_company.el --- Company settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)


;; Company: Modular in-buffer completion framework for Emacs
;; https://company-mode.github.io/
(leaf company
  :ensure t
  :bind (("C-M-i" . company-complete)
         (company-active-map
          ("C-n" . company-select-next))
         (company-active-map
          ("C-p" . company-select-previous))
         (company-active-map
          ("C-s" . company-filter-candidates))
         (company-active-map
          ("C-i" . company-complete-selection))
         (company-active-map
          ([tab]
           . company-complete-selection)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 3)
           (company-selection-wrap-around . t)
           (completion-ignore-case . t))
  :require t
  :config
  (global-company-mode)
  (push 'company-lsp company-backends))


;; company-box
(leaf company-box
  :ensure t
  :hook (company-mode-hook))


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; 12_company.el ends here

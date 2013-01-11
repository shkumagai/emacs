;; Exuberant ctags
(when (require 'ctags nil t)
  (setq tags-revert-without-query t)
  ;; Command-line to call `ctags'.
  (setq ctags-command "/opt/local/bin/ctags -R -e ")
  ;; Comment out when anything-exuberant-ctags.el not in use.
  ;; (setq ctags-command "ctags -R --fields=\"+afikKlmnsSzt\" ")
  (global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)
  )
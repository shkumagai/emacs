;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Multi-web-mode
(when (require 'multi-web-mode nil t)
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags '((js2-mode "<script+\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>"
                              "</script>")
                    (css-mode "<style+type=\"text/css\"[^>]*>" "</style>")
                    ))
  (setq mweb-filename-extensions '("html" "htm" "php" "php4" "php5"))
  (multi-web-global-mode 1))

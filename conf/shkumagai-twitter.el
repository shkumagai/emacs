;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; twittering-mode
(when (require 'twittering-mode nil t)
  (setq twittering-username "shkumagai")
  (setq twittering-private-info-file (expand-file-name
                                      (concat user-emacs-directory ".twittering-mode.gpg")))
  ;; (setq twittering-status-format "%i @%s / %S %p: \n %T\n [%@]%r %R %f%L\n")
  ;; (setq twittering-retweet-format " RT @%s: %t")
  (setq twittering-use-ssl t)
  (setq twittering-icon-mode t)
  (setq twittering-timer-interval 60)
  ;; (setq twittering-convert-fix-size 48)
  ;; (setq twittering-update-status-function 'twittering-update-status-from-pop-up-buffer)

  ;; ;; 起動時に読み込む timeline
  ;; (setq twittering-initial-timeline-spec-string
  ;;       '(":home"
  ;;         ":replies"))

  ;; ;; 認証情報
  (setq twittering-use-master-password t)

  ;; キーバインド
  (add-hook 'twittering-mode-hook
            '(lambda ()
               (define-key twittering-mode-map (kbd "F") 'twittering-favorite)
               (define-key twittering-mode-map (kbd "R") 'twittering-reply-to-user)
               (define-key twittering-mode-map (kbd "Q") 'twittering-organic-retweet)
               (define-key twittering-mode-map (kbd "T") 'twittering-native-retweet)
               (define-key twittering-mode-map (kbd "M") 'twittering-direct-message)
               (define-key twittering-mode-map (kbd "N") 'twittering-update-status-interactive)
               (define-key twittering-mode-map (kbd "C-c C-f") 'twittering-home-timeline)))
  )

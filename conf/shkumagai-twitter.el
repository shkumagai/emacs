;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-to-column: nil -*-

;; twittering-mode
(when (require 'twittering-mode nil t)
  (setq twittering-username "shkumagai")
  (setq twittering-status-format "%i @%s / %S %p: \n %T\n [%@]%r %R %f%L\n")
  (setq twittering-retweet-format " RT @%s: %t")
  (setq twittering-use-ssl nil)
  (setq twittering-icon-mode nil)
  (setq twittering-scroll-mode nil)
  (setq twittering-convert-fix-size 48)
  (setq twittering-timer-interval 60)
  (setq twittering-update-status-function 'twittering-update-status-from-pop-up-buffer)

  ;; 起動時に読み込む timeline
  (setq twittering-initial-timeline-spec-string
        '(":home"
          ":replies"))

  ;; 認証情報
  (setq twittering-auth-method 'oauth)
  (setq twittering-account-authorization 'authorized)
  (setq twittering-oauth-customer-key "*** oauth customer key ***")
  (setq twittering-oauth-customer-secert "*** oauth customer secret ***")
  (setq twittering-oauth-accese-token-alist
        '(("screen_name" . "shkumagai"
           "user_id" . ""
           "oauth_token" . "*** oauth token ***"
           "oauth_token_secret" . "*** oauth token secret ***")))

  ;; 短縮URLに bit.ly を使用する
  (add-to-list 'twittering-tinyurl-services-map
               '(bitly . "http://api.bit.ly/shorten?version=2.0.1&login=*** login ***&apiKey=*** api key ***&format=text&longUrl="))
  (setq twittering-tinyurl-service 'bitly)

  ;; キーバインド
  (add-hook 'twittering-mode-hook
            '(lambda ()
               (define-key twittering-mode-map (kbd "F") 'twittering-favorite)
               (define-key twittering-mode-map (kbd "R") 'twittering-reply-to-user)
               (define-key twittering-mode-map (kbd "Q") 'twittering-organic-retweet)
               (define-key twittering-mode-map (kbd "T") 'twittering-native-retweet)
               (define-key twittering-mode-map (kbd "M") 'twittering-direct-message)
               (define-key twittering-mode-map (kbd "N") 'twittering-update-status-interactive)
               (define-key twittering-mode-map (kbd "C-c C-f") 'twittering-home-timeline)
               ))

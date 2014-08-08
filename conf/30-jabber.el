;; -*- coding: utf-8-unix; mode: emacs-list; indent-tabs-mode: nil -*-

(require 'jabber-autoloads)

;; HipChat
(defvar hipchat-number "117451")
(defvar hipchat-full-number (concat hipchat-number "_865334"))
(defvar hipchat-nickname "Shoji Kumagai")
(defvar hipchat-server "chat.hipchat.com")
(defvar hipchat-conf-server "conf.hipchat.com")
(defvar hipchat-full-name (concat hipchat-full-number "@" hipchat-server))

(setq jabber-username hipchat-full-number)
(setq jabber-server hipchat-server)
(setq jabber-account-list '((hipchat-full-name)))

;; Join a room
(defun hipchat-join (room)
  (interactive "sRoom name: ")
  (jabber-groupchat-join
   (jabber-read-account)
   (concat hipchat-number "_" room "@" hipchat-conf-server)
   hipchat-nickname
   t))

;; Mention
(defun hipchat-mention (nickname)
  (interactive
   (list (jabber-muc-read-nickname jabber-group "Nickname: ")))
  (insert (concat "@\"" nickname "\" ")))

;; Noticication
(defun jabber-notify-send (FROM BUFFER TEXT TITLE)
  (start-process-shell-command "notify-send" nil (concat "notify-send --urgency=critical \"" FROM " : " TEXT "\"")))

(setq jabber-alert-message-hooks
      '(jabber-notify-send))


;; specify browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "aurora")

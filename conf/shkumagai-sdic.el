;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; sdic w/z Eijiro ver.5.0

;;; http://d.hatena.ne.jp/zqwell-ss/20091205/1260037903
;;; http://d.hatena.ne.jp/eiel/20090111#1231681381
;;; http://nox-insomniae.ddo.jp/insomnia/2009/01/eijiro-emacs.html
;;; http://d.hatena.ne.jp/kyagi/20090515/1242379726

(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key (kbd "C-c w") 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point "sdic" "カーソル位置の英単語の意味を調べる" t nil)
(global-set-key (kbd "C-c W") 'sdic-describe-word-at-point)

(setq sdic-eiwa-dictionary-list
      '((sdicf-client "~/work/dictionaries/eijiro118.sdic")))
(setq sdic-waei-dictionary-list
      '((sdicf-client "~/work/dictionaries/waeiji118.sdic"
                      (add-keys-to-headword t))))
(setq sdic-default-coding-system 'utf-8-unix)
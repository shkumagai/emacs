;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; How to setting up on MacOSX
;;
;; Install Erlang
;;    I use Erlang executable which built from source.
;;    Installation steps in case of R15B is here.
;;
;;    1. Download source archive from erlang.org.
;;
;;       $ curl -O http://www.erlang.org/download/otp_src_R15B.tar.gz
;;
;;    2. Untar the archive, then change directory into there.
;;
;;       $ cd otp_src_R15B
;;
;;    3. Follow the CMMI (configure, make, make install) steps,
;;       you should install into your system :)
;;
;;       $ ./configure --prefix=/opt/local/erlang/R15B <OTHER_OPTIONS>
;;       $ make
;;       $ sudo make install

;; Erlang mode
;; R15B
;; (setq erlang-root-dir "/opt/local/erlang/R15B")
;; (add-to-list 'load-path (concat erlang-root-dir "/lib/erlang/lib/tools-2.6.6.6/emacs"))
;; R15B01
;; (setq erlang-root-dir "/opt/local/erlang/R15B01")
;; (add-to-list 'load-path (concat erlang-root-dir "/lib/erlang/lib/tools-2.6.7/emacs"))
;; R15B02
;; (setq erlang-root-dir "/opt/local/erlang/R15B02")
;; (add-to-list 'load-path (concat erlang-root-dir "/lib/erlang/lib/tools-2.6.8/emacs"))
;; R15B03-1
;; (setq erlang-root-dir "/opt/local/erlang/R15B03-1")
;; (add-to-list 'load-path (concat erlang-root-dir "/lib/erlang/lib/tools-2.6.8/emacs"))
;; R16A
;; (setq erlang-root-dir "/opt/local/erlang/R16A")
;; (add-to-list 'load-path (concat erlang-root-dir "/lib/erlang/lib/tools-2.6.9/emacs"))
;; R16B
;; (setq erlang-root-dir "/opt/local/erlang/R16B")
;; (add-to-list 'load-path (concat erlang-root-dir "/lib/erlang/lib/tools-2.6.10/emacs"))
(setq erlang-root-dir "/usr/lib64/erlang")
(add-to-list 'load-path (concat erlang-root-dir "/lib/tools-2.6.8/emacs"))

(add-to-list 'exec-path (concat erlang-root-dir "/bin"))
(require 'erlang-start)

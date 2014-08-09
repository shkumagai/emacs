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
;;
;; $ find /usr/lib /usr/lib64 /opt/local/lib -type d -name "erlang"
(when (eq window-system 'ns)
  (setq erlang-root-dir "/opt/local/lib/erlang"))

(when (eq window-system 'x)
  (setq erlang-root-dir "/usr/lib64/erlang"))

;; $ find /usr/lib /usr/lib64 /opt/local/lib -type d -name "emacs"
(add-to-list 'load-path (concat erlang-root-dir "/lib/tools-2.6.15/emacs"))

(add-to-list 'exec-path (concat erlang-root-dir "/bin"))
(require 'erlang-start)

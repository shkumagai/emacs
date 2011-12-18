;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; How to seting up on MacOSX
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
(add-to-list 'load-path
             "/opt/local/erlang/R15B/lib/erlang/lib/tools-2.6.6.6/emacs")
(setq erlang-root-dir "/opt/local/erlang/R15B")
(add-to-list 'exec-path "/opt/local/erlang/R15B/bin")
(require 'erlang-start)

;; Install Distel
;;    I used SVN trunk to install the Distel package.
;;    First of all, you'll need to check it out from the repository using the
;;    following command:
;;
;;    $ git clone git://github.com/massemanet/distel.git
;;
;;    And then, you'll need to follow INSTALL instruction to build and update
;;    .emacs file.

;; For more informations, refer to these blogs.
;;
;; http://

;; distel
(add-to-list 'load-path "/usr/local/share/distel/elisp")
(require 'distel)
(distel-setup)

(add-hook 'erlang-mode-hook
          (lambda ()
            ;; when starting an Erlang shell in Emacs,
            ;; default in the node name
            (setq inferior-erlang-machine-options '("-sname" "emacs"))
            ;; add Erlang functions to an imenu menu
            (imenu-add-to-menubar "imenu")
            ))

;; tel distel to default to that node
(setq erl-nodename-cache
      (make-symbol
       (concat
        "emacs@"
        ;; Mac OS X uses "name.local" instead of "name",
        ;; this should work pretty much anywhere without having
        ;; to muck with NetInfo
        ;; ... but I only it on Mac OS X.
        (car (split-string
              (shell-command-to-string "hostname"))))))

;; In additional
;; ;; A number of the erlang-extended-mode key bindings are useful
;; ;; in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    ;; add some Distel bindings to the Erlang shell
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

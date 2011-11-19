;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; How to seting up on MacOSX
;;
;; Install Erlang
;;    I used the version of Erlang and Erlang-mode for Emacs in MacPorts.
;;    MacPorts was install Erlang under /opt/local.

;; Erlang mode
(add-to-list 'load-path "/opt/local/lib/erlang/lib/tools-2.6.6.5/emacs")
(setq erlang-root-dir "/opt/local/lib/erlang")
(add-to-list 'exec-path "/opt/local/lib/erlang/bin")
(setq erlang-root-dir "/opt/local/lib/erlang")
(require 'erlang-start)

;; Install Distel
;;    I used SVN trunk to install the Distel package.
;;    First of all, you'll need to check it out from the repository using the
;;    following command:
;;
;;    $ svn checkout http://distel.googlecode.com/svn/trunk/ distel
;;
;;    And then, you'll need to follow INSTALL instruction to build and update
;;    .emacs file.

;; For more informations, refer to these blogs.
;;
;; http://parijatmishra.wordpress.com/2008/08/15/up-and-running-with-emacs-erlang-and-distel/
;; http://bc.tech.coop/blog/070528.html

;; distel
(add-to-list 'load-path "/usr/local/share/distel/elisp")
(require 'distel)
(distel-setup)

(add-hook 'erlang-mode-hook
          (lambda ()
            ;; when starting an Erlang shell in Emacs, default in the node name
            (setq inferior-erlang-machine-options '("-sname" "emacs"))
            ;; add Erlang functions to an imenu menu
            (imenu-add-to-menubar "imenu")))

;; In additional
;; ;; A number of the erlang-extended-mode key bindings are useful in the shell too
;; (defconst distel-shell-keys
;;   '(("\C-\M-i"   erl-complete)
;;     ("\M-?"      erl-complete)	
;;     ("\M-."      erl-find-source-under-point)
;;     ("\M-,"      erl-find-source-unwind) 
;;     ("\M-*"      erl-find-source-unwind) 
;;     )
;;   "Additional keys to bind when in Erlang shell.")

;; (add-hook 'erlang-shell-mode-hook
;; 	  (lambda ()
;; 	    ;; add some Distel bindings to the Erlang shell
;; 	    (dolist (spec distel-shell-keys)
;; 	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

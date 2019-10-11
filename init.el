;;; init.el --- My Emacs configuration file  -*- mode: lexical-bindings: t -*-

;; Filename: init.el
;; Description: My Emacs configuration file
;; Package-Requires: ((Emacs "26.1"))
;; Author: KUMAGAI, Shoji <take.this.t.your.grave_at_gmail.com>
;; Created: 2011-11-17
;; Modified: 2019-03-05
;; URL: https://github.com/shkumagai/emacs/init.el

;;; Commentary:

;;;;; External dependency installation
;;
;; - C/Migemo
;;   % brew install migemo
;;
;; - gettext
;;   % brew install gettext

;;; Code:

(eval-when-compile (require 'cl))
(setq init-file-debug t)
(cd "~/")  ; move to home directory.
(setq force-load-messages t)
(message (format "Startup time: %s" (format-time-string "%Y/%m/%d %H:%M:%S")))

;;;; Macros/Functions
;;;;; Add paths to load-path
(defun add-to-load-path (&rest paths)
  "Add specified PATHS to 'load-path'."
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name
                                (concat user-emacs-directory path))))
    (add-to-list 'load-path default-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (normal-top-level-add-subdirs-to-load-path))))))

;;;;; Find installed path name
(defun installed-dir (package)
  "Return directory path where package with PACKAGE name is installed."
  (let ((dname
         (car (loop for p in (directory-files package-user-dir)
                    when (string-match (concat package "-*") p)
                    collect p))))
    (concat package-user-dir "/" dname)))

;;;;; Insert timestamp
(setq system-time-locale "C")
(defvar current-date-time-format "%b %d, %Y %a %T"
  "Format of date to insert `insert-current-date-time' function.
See help of `format-time-string' for possible replacement")

(defvar current-time-format "%T"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

;; Insert current date and time
(defun insert-current-date-time ()
  "Insert current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  ;; (insert "==========\n")
  (insert (format-time-string current-date-time-format (current-time))))

;; Insert current time
(defun insert-current-time ()
  "Insert current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))

;; Add key bindings
(global-set-key (kbd "C-c D") 'insert-current-date-time)
(global-set-key (kbd "C-c T") 'insert-current-time)

;;;;; Create new temporary buffer named "*temp*"
;; reference url:
;; - http://d.hatena.ne.jp/noqisofon/20101102/1288647885
(defun create-temporary-buffer ()
  "Create and show new temporary buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*temp*"))
  (setq buffer-offer-save nil))
(global-set-key (kbd "C-c C-c t") 'create-temporary-buffer)

;;;; Initialize

;;;;; add-to-load-path
(add-to-load-path "elisp")

;;;;; package
(require 'package)
(add-to-list
 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list
;;  'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(unless (file-directory-p (concat user-emacs-directory "elpa/archives"))
  (package-list-packages))
(package-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;; Install packages automatically by package.el
;; refer to: http://blog.64p.org/entry/2013/05/01/233306
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
   package-selected-packages)

(dolist (buf '("*Packages*" "*Compile-Log*"))
  (cond ((get-buffer buf)
         (kill-buffer buf))))

;;;; Languages
;; Settings arround coding-system, input-method for each environment.
;;
;; Support:
;; - MacOSX
;; - Windows
;; - Linux
;;;;; Global
(define-coding-system-alias 'UTF-8 'utf-8-unix)
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;;;;; MacOSX
(when (eq window-system 'ns)
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;;;;; Windows
(when (eq window-system 'w32)
  (defvar w32-ime-mode-line-state-indicator-list nil)
  (setq default-input-method "W32-IME")

  (setq-default w32-ime-mode-line-state-indicator "[--]")
  (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  (w32-ime-initialize)
  (global-set-key [M-kanji] 'ignore)

  (setq file-name-coding-system 'cp932)
  (setq keyboard-coding-system 'cp932)
  (setq locale-coding-system 'cp932))

;;;;; Linux and Other XWindow System (ex. xBSD)
(when (eq window-system 'x)
  (defvar mozc-candidate-style nil)
  (use-package mozc)
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'echo-area)

  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))

;;;; Exec path setting
;; Add PATH Environment Variables to exec path from shell
(use-package exec-path-from-shell
  :if (not (eq window-system 'w32))
  :config
  (exec-path-from-shell-initialize))

;;;; Font setting
;; Settings arround fonts for each environment.
;;
;; Support:
;; - MacOSX
;; - Windows
;; - Linux
;;;;; Mac OX
(when (eq window-system 'ns)
  (let* ((size 12)
         (h (* size 10))
         (asciifont "Noto Sans Mono CJK JP")
         (jpfont "Noto Sans Mono CJK JP")
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; 半角カナ
    (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec))  ; ギリシャ文字
  ;; define aspect ratio
  (dolist (elt '((".*Noto Sans Mono CJK JP.*" . 1.0)))
    (add-to-list 'face-font-rescale-alist elt)))

;;;;; Windows
(when (eq window-system 'w32)
  (let* ((size 11)
         (h (* size 10))
         (asciifont "Consolas")
         (jpfont "MeiryoKe_Console")
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; 半角カナ
    (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec)) ; ギリシャ文字
  ;; define aspect ratio
  (dolist (elt '((".*Consolas.*" . 1.0)
                 (".*MeiryoKe_Console.*" . 1.0)))
    (add-to-list 'face-font-rescale-alist elt)))

;;;;; Linux
(when (eq window-system 'x)
  (let* ((size 10)
         (h (* size 10))
         (asciifont "MigMix1M")
         (jpfont "Ricty")
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; 半角カナ
    (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec)) ; ギリシャ文字
  ;; define aspect ratio
  (dolist (elt '((".*MigMix1M.*" . 1.0)
                 (".*Ricty.*" . 1.1)))
    (add-to-list 'face-font-rescale-alist elt)))

;;;; Faces

;;;;; Visibility
(setq inhibit-startup-screen t)
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(blink-cursor-mode t)
(menu-bar-mode 0)
(column-number-mode 1)
(setq default-frame-alist
      '((width . 120) (height . 51)))

;;;;; Show absolute path on title bar
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;;;;; Parenthesis
(use-package paren
  :ensure nil
  :hook
  (after-init . show-paren-mode)
  :custom-face
  (show-paren-mode ((nil (:background "#44475a" :foreground "#f1fa8c"))))
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;;;;; Beacon
;; https://github.com/Malabarba/beacon
(use-package beacon
  :custom
  (beacon-size 60)
  (beacon-color "yellow")
  (beacon-blink-delay 0.5)
  (beacon-blink-dulation 0.5)
  :config (beacon-mode 1))

;;;;; Indent highlighting
;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enable t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-character ?\|)
  (highlight-indent-guides-method 'character))  ; column

;;;;; Change background color in region
(set-face-background 'region "darkgreen")
(setq frame-background-mode 'dark)

;;;;; Frame transparency
(set-frame-parameter (selected-frame) 'alpha '(95 80))

;;;;; Remove trailing whitespace on save file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;; Toggle fullscreen mode
(define-key global-map (kbd "M-RET") 'ns-toggle-fullscreen)

;;;;; Tab stops
(defvar default-tab-width nil)
(defun gen-tab-stop (&optional width max)
  "Return a sequence suitable for `tab-stop-list' based on WIDTH and MAX."
  (let* ((max-column (or max 200))
         (tab-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq tab-stop-list (gen-tab-stop))

;;;;; Line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;;;;; Whitespace mode
(use-package whitespace
  :bind (("C-x w" . global-whitespace-mode))
  :custom
  (whitespace-style '(face trailing tabs spaces empty tab-mark space-mark))
  (whitespace-display-mappings
   '((space-mark ?\u3000 [?\u25a1])
     (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
  (whitespace-space-regexp "\\( +\\|\u3000+\\)")
  :config
  (global-whitespace-mode 1)
  (defvar my/bg-color "#2d3743")
  (set-face-attribute 'whitespace-trailing nil :background my/bg-color :foreground "DeepPink" :underline t)
  (set-face-attribute 'whitespace-tab nil :background my/bg-color :foreground "LightSkyBlue" :underline t)
  (set-face-attribute 'whitespace-space nil :background my/bg-color :foreground "GreenYellow" :weight 'bold)
  (set-face-attribute 'whitespace-empty nil :background my/bg-color))

;;;; Color-Theme
;;;;; doom-themes
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;;;; Modeline
;;;;; doom-modeline
(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)             ; Show 'all-the-icons' or not
  (doom-modeline-major-mode-icon t)  ; Display color icon for 'major-mode'
  (doom-modeline-minor-modes nil)    ; Don't display minor-mode
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 0)
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))

;;;; Neotree: A emacs tree plugin like NERD tree for Vim.
;; https://github.com/jaypei/emacs-neotree
(use-package neotree
  :bind
  ("<f8>" . neotree-toggle)
  :defines
  neo-persist-show
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrows))
  (setq neo-persist-show t)
  (setq neo-mode-line-type 'none)
  (setq neo-smart-open t)
  (setq neo-window-width 45)
  ;; Disable line numbers minor mode for neotree
  (add-hook 'neo-after-create-hook (lambda (&rest _) (display-line-numbers-mode -1))))

;;;; Miscellaneous settings

;;;;; Key bindings
(define-key global-map (kbd "C-x C-h") 'help)
(define-key global-map (kbd "C-h") 'describe-bindings)

;;;;; which-key
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;;;;; Check Dropbox
(defvar my:check-dropbox (file-exists-p (concat (getenv "HOME") "/Dropbox")))
(if my:check-dropbox (defvar my:dropbox (concat (getenv "HOME") "/Dropbox/")))

;;;;; lisp-mode-hook
(defun lisp-mode-hooks ()
  "'lisp-mode-hooks'."
  (use-package eldoc
    :config
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))

(add-hook 'emacs-lisp-mode-hook 'lisp-mode-hooks)
(add-hook 'lisp-interaction-mode-hook 'lisp-mode-hooks)
(add-hook 'ielm-mode-hook 'lisp-mode-hooks)

;;;; History Handling
;;;;; undo-tree: visualize undo branches
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;;;;; undohist: enable undo on closed buffer
;; https://github.com/emacsmirror/undohist
(use-package undohist
  :config
  (undohist-initialize))

;;;; Multiple-Cursors
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/skip-all-like-this)))

;;;; Org-mode
(use-package org
  :ensure t
  :defines org-capture-templates
  :custom
  ; general
  (org-directory "~/Dropbox/Org")
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w)" "BACKLOG(b)" "|" "DONE(d)" "CANCELED(c@)")))
  (org-log-done 'time)
  (org-tag-alist '(
                   ("office" . ?o)    ; Task区分: オフィス
                   ("private" , ?p)   ; Task区分: 個人, 自宅
                   ("study" . ?s)     ; Task種別: 勉強, 自習
                   ("meeting" . ?m)   ; Task種別: ミーティング
                   ("writing" . ?d)   ; Task種別: ライティング（書き物）
                   ("travel" . ?t)    ; Task種別: 旅行, 出張等
                   ))
  ; capture
  (org-capture-templates
   '(("c" "Codereading" entry (file+headline "~/Dropbox/Org/codereading.org" "CodeReading")
      "* %?\n  %i\n  %a\n  Added on %U")
     ("k" "Knowledge" entry (file+headline "~/Dropbox/Org/knowledge.org" "Knowledge")
      "* %?\n  Added on %U")
     ("e" "Event" entry (file+headline "~/Dropbox/Org/events.org" "Events")
      "* %?\n  Added on %U")
     ("t" "Todo" entry (file+headline "~/Dropbox/Org/todos.org" "Todos")
      "* TODO %?\n  Added on %U\n %i")))
  ; agenda
  (org-agenda-files '("~/Dropbox/Org/events.org" "~/Dropbox/Org/todos.org" "~/Dropbox/Org/projects"))
  (org-agenda-current-time-string "← now")
  (org-agenda-time-grid
   '((daily today require-timed)
     (0900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
     "-"
     "----------------"))
  (hl-line-face 'underline)
  (calendar-holidays nil)
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  :custom-face
  (org-link ((t (:foreground "#ebe087" :underline t))))
  (org-list-dt ((t (:foreground "#bd93f9"))))
  (org-special-keyword ((t (:foreground "#6272a4"))))
  (org-todo ((t (:background "#272934" :foreground "#51fa7b" :weight bold))))
  (org-document-title ((t (:foreground "#f1fa8c" :weight bold))))
  (org-done ((t (:background "#373844" :foreground "#216933" :strike-through nil :weight bold))))
  (org-footnote ((t (:foreground "#76e0f3"))))
  :config
  (define-key global-map (kbd "C-c c") 'org-capture)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
  ;; Need to install Nerd fonts https://github.com/ryanoasis/nerd-fonts
  ;; Using Homebrew:
  ;;   $ brew tap homebrew/cask-fonts
  ;;   $ brew cask install font-hack-nerd-font-mono
  ;;
  (use-package org-bullets
    :custom (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" ""))
    :hook (org-mode . org-bullets-mode)))

;;;; Flycheck: A modern on-the-fly syntax checking extension, and a modern alternative to Flymake.
;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;;; Anzu: an Emacs port of anzu.vim
;; https://github.com/syohex/emacs-anzu
(use-package anzu
  :config
  (global-anzu-mode +1))

;;;; LSP -- language server protorol mode
;;;;; lsp-mode
(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  (lsp-response-timeout 5)
  (lsp-prefer-flymake 'flymake)
  :hook (prog-major-mode . lsp-prog-major-mode-enable))

;;;;; lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :custom
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck-enable nil)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions nil)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable nil)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand)  ;; never, on-demand, or always
  :bind
  (:map lsp-mode-map
        ("C-c C-r" . lsp-ui-peek-find-references)
        ("C-c C-j" . lsp-ui-peek-find-definitions)
        ("C-c i" . lsp-ui-peed-find-implementation)
        ("C-c m" . lsp-ui-imenu)
        ("C-c s" . lsp-ui-sideline-mode))
  :hook (lsp-mode . lsp-ui-mode))

;;;;; company-lsp
(use-package company-lsp
  :after (lsp-mode company)
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates t)  ;; always using cache
  (company-lsp-async t)
  (company-lsp-enable-recompletion nil))

;;;; Company: Modular in-buffer completion framework for Emacs
;; https://company-mode.github.io/
(use-package company
  :custom
  (company-idle-delay 0)  ; default: 0.5
  (company-minimum-prefix-length 3)  ; default: 4
  (company-selection-wrap-around t)
  (completion-ignore-case t)
  :config
  (global-company-mode)
  (push 'company-lsp company-backends)  ; add company-lsp as backend
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)  ;; filter by C-s
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)
  (define-key company-active-map [tab] 'company-complete-selection))

;;;;; company-box
(use-package company-box
  :hook (company-mode . company-box-mode))

;;;; Ivy/Counsel/Swiper
;; https://github.com/abo-abo/swiper
;;   This repository contains:
;;   Ivy, a generic completion mechanism for Emacs.
;;   Counsel, a collection of Ivy-enhanced versions of common Emacs commands.
;;   Swiper, an Ivy-enhanced alternative to isearch.
(use-package counsel
  :ensure t
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-height 30)  ;; extends minibuffer size (important!)
  (ivy-extra-directories nil)
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c f") 'counsel-recentf)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox))

;;;; ivy-rich
(use-package ivy-rich
  :after ivy
  :config (ivy-rich-mode 1))

;;;; all-the-icons-ivy
(use-package all-the-icons-ivy
  :ensure t
  :config (all-the-icons-ivy-setup))

;;;; dumb-jump: jump to definition for multiple languages without configuration.
;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :config
  (setq dumb-jump-mode t)
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-use-visible-window nil)

  (define-key global-map [(super d)] 'dumb-jump-go)  ;; go-to-definition
  (define-key global-map [(super shift d)] 'dumb-jump-back))

;;;; Migemo: Japanese increment search with 'Romanization of Japanese'(ローマ字).
;; https://github.com/emacs-jp/migemo
(use-package migemo
  :if (executable-find "cmigemo")
  :custom
  (migemo-command "cmigemo")
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  ;; cache setting
  (migemo-use-pattern-alist t)
  (migemo-use-frequent-pattern-alist t)
  (migemo-pattern-alist-length 1000)
  (migemo-coding-system 'utf-8-unix)
  :config
  (cond
   ((eq system-type 'darwin)
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
   ((eq system-type 'gnu/linux)
    (setq migemo-options '("-q" "--emacs" "-i" "\a"))
    (setq migemo-dictionary "/usr/share/migemo/migemo-dict")))
  (migemo-init))

;;;; Magit: an interface to the version control system Git.
;; https://github.com/magit/magit
(use-package git
  :bind (("C-c C-g" . magit-status)))

(use-package git-gutter
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  :custom-face
  (git-gutter:modified ((t (:background "#f1fa8c"))))
  (git-gutter:added    ((t (:background "#50fa7b"))))
  (git-gutter:deleted  ((t (:background "#ff79c6"))))
  :config
  (global-git-gutter-mode +1))

;;;; Perl
;;;;; custumizing cperl-mode
(defalias 'perl-mode 'cperl-mode)
(setq auto-mode-alist
      (cons '("\\.t$" . cperl-mode) auto-mode-alist))
(custom-set-variables '(cperl-indent-level 4)
                      '(cperl-continuted-statement-offset 4)
                      '(cperl-close-paren-offset -4)
                      '(cperl-level-offset -4)
                      '(cperl-comment-column 40)
                      '(cperl-highlight-variables-indiscriminaly t)
                      '(cperl-indent-parens-as-block t)
                      '(cperl-tab-always-indent nil)
                      '(cperl-font-lock t))
(add-hook 'cperl-mode-hook
          '(lambda ()
             (progn
               (setq indent-tabs-mode nil)
               (setq tab-width nil)
               (cperl-set-style "PerlStyle")

               ;; perl completion
               (use-package auto-completion)
               (use-package perl-completion)
               (add-to-list 'ac-source 'ac-source-perl-completion)
               (perl-completion-mode t))))

;;;;; perltidy
(defmacro mark-active ()
  "XEmacs/Emacs compatibility macro."
  (if (boundp 'mark-active)
      'mark-active
    '(mark)))

(defun perltidy ()
  "Run perltidy on the current region or buffer."
  (interactive)
  ;; Inexplicably. save-excursion doesn't work here.
  (let ((orig-point (point)))
    (unless (mark-active) (mark-defun))
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)
    (goto-char orig-point)))

(global-set-key (kbd "C-c t") 'perltidy)

(defvar perltidy-mode nil
  "Automatically 'perltidy' when saving.")
(make-variable-buffer-local 'perltidy-mode)
(defun perltidy-write-hook ()
  "Perltidy a buffer during 'write-file-hooks' for 'perltidy-mode'."
  (if perltidy-mode
      (save-excursion
        (widen)
        (mark-whole-buffer)
        (not (perltidy)))
    nil))
(defun perltidy-mode (&optional arg)
  "Perltidy minor mode with ARG."
  (interactive "P")
  (setq perltidy-mode
        (if (null arg)
            (not perltidy-mode)
          (> (prefix-numeric-value-arg) 0)))
  (mark-local-hook 'write-file-hooks)
  (if perltidy-mode
      (add-hook 'write-file-hooks 'perltidy-write-hook)
    (remove-hook 'write-file-hooks 'perltidy-write-hook)))
(if (not (assq 'perltidy-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(perltidy-mode " Perltidy")
                minor-mode-alist)))
(eval-after-load "cperl-mode"
  '(add-hook 'cperl-mode-hook 'perltidy-mode))

;;;; Python
;;;;; Base settings
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(add-hook 'python-mode-hook #'lsp)

;; ;;;;; jedi
;; ;; https://github.com/tkf/emacs-jedi
;; (use-package jedi-core
;;   :config
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:use-shortcuts t)
;;   (defun my/python-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'my/python-mode-hook))

;;;;; virtualenvwrapper
;; https://github.com/porterjamesj/virtualenvwrapper.el
(use-package virtualenvwrapper
  :config
  (setq venv-location "~/.virtualenvs"))

;;;; JavaScript

;;;;; js2-mode
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\(_t\\)\\'" . js2-mode)))

;;;;; React jsx-mode
(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode)))

;;;; TypeScript
;; https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

;;;; Common Lisp
;; Setup load-path, autoloads and your Lisp system.
;; (add-to-list 'load-path "~/.emacs.d/elisp/slime")
;; (use-package slime-autoloads
;;   :config
;;   (setq inferior-lisp-program "sbcl")
;;   (setq slime-net-coding-system 'utf-8-unix)
;;   (slime-setup '(slime-repl slime-fancy slime-banner slime-indentation slime-company))
;;   )

;;;; HTML and Psedo HTML (for template language files)
(use-package web-mode
  :config
  (when (< emacs-major-version 24)
    (defalias 'prog-mode 'fundamental-mode))

  (add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

  ;; indents
  (defun web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset    2)
    (setq web-mode-code-indent-offset   4))
  (add-hook 'web-mode-hook 'web-mode-hook))

;;;; CSS
(use-package css-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.css\\(_t\\)\\'" . css-mode)))

;;;; YAML
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode)))

;;;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;; reStructuredtext
(use-package rst
  :config
  (add-to-list 'auto-mode-alist '("\\.rse?t$" . rst-mode)))

;;;; PO file (from gettext)
(use-package po-mode
  :config
  (setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(max-specpdl-size 20000)
 '(max-lisp-eval-depth 20000)
 '(safe-local-variable-values (quote ((syntax . elisp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here

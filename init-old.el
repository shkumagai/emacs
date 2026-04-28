;;; init.el --- My Emacs configuration file  -*- lexical-binding: t -*-

;; Filename: init.el
;; Description: My Emacs configuration file
;; Package-Requires: ((Emacs "27.1"))
;; Author: KUMAGAI, Shoji <take.this.t.your.grave_at_gmail.com>
;; Created: Nov 17, 2011
;; Modified: Mar 13, 2022 Sun 22:38:51
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

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    (leaf bind-key :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))


;; Quiet startup
(when window-system
  (scroll-bar-mode 0)
  (tool-bar-mode 0))
(menu-bar-mode 0)
(blink-cursor-mode t)
(column-number-mode 1)

(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(setq frame-title-format nil)
(setq ring-bell-function 'ignore)

(setq delete-by-moving-to-trash t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq set-mark-command-repeat-pop t)
(setq track-eol t)
(setq line-move-visual t)
(setq indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)


(leaf server
  :doc "Start the server in Emacs session"
  :require t
  :config
  (unless (server-running-p)
    (server-start)))


(leaf exec-path-from-shell
  :doc "ensure environment variable inside Emacs looks"
  :url "https://github.com/purcell/exec-path-from-shell"
  :ensure t
  :when (memq window-system '(mac ns x))
  :hook (emacs-startup-hook . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments . '("-l")) ; default: ("-l" "-i")
  :config
  (setq exec-path-from-shell-check-startup-files nil))


;; Local elisp
(add-to-list 'load-path (expand-file-name
			 (concat user-emacs-directory "elisp")))


;; Change background color in region
(set-face-background 'region "darkgreen")
(setq frame-background-mode 'dark)


;; frame-transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 75))


;; Remove trailing whitespace on file save
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Key bindings
(bind-key "C-x C-h" 'help global-map)
(bind-key "C-h" 'describe-bindings global-map)


;; Tab stops
(defvar default-tab-width nil)
(defun gen-tab-stop (&optional width max)
  "Return a sequence suitable for `tab-stop-list' based on WIDTH and MAX."
  (let* ((max-column (or max 200))
         (tag-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

;; (set-default tab-width 4)
(setq default-tab-width 4)
(setq tab-stop-list (gen-tab-stop))


;; Line numbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))


;; Default encodings
(set-language-environment "Japanese")
(set-locale-environment "en_US.UTF-8")

(define-coding-system-alias 'UTF-8 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(setq buffer-file-coding-system 'utf-8)
(setq save-buffer-coding-system 'utf-8-unix)
(setq process-coding-system-alist
      (cons '("grep" utf-8 . utf-8) process-coding-system-alist))


;; MacOSX
(when (eq window-system 'ns)
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;; Windows
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

;; Linux and Other XWindow System (ex. xBSD)
(when (eq window-system 'x)
  (defvar mozc-candidate-style nil)
  (leaf mozc
    :ensure t
    :require t)
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'echo-area)

  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))


;; Fonts
;; for MacOS
(when (eq window-system 'ns)
  (let* ((size 12)
         (h (* size 12))
         (asciifont "Noto Sans Mono")
         (jpfont "Noto Sans JP")
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)))
    (set-face-attribute 'default nil :family asciifont :height h)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; 半角カナ
    (set-fontset-font nil '(#x0080 . #x024f) fontspec) ; 分音符付きラテン
    (set-fontset-font nil '(#x0370 . #x03ff) fontspec)) ; ギリシャ文字
  (dolist (elt '((".*Noto Sans.*" . 1.0)))
    (add-to-list 'face-font-rescale-alist elt)))

;; for Windows
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

;; for Linux
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


;; Locale
(setq system-time-locale "C")


;; Insert timestamp formats
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


;; Create new temporary buffer named "*temp*"
;; reference url:
;; - http://d.hatena.ne.jp/noqisofon/20101102/1288647885
(defun create-temporary-buffer ()
  "Create and show new temporary buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*temp*"))
  (setq buffer-offer-save nil))

(global-set-key (kbd "C-c C-c t") 'create-temporary-buffer)


;; Extentions
(leaf which-key
  :doc "Display available keybindings in popup"
  :url "https://github.com/justbur/emacs-which-key"
  :ensure t
  :hook (after-init-hook . which-key-mode))


(leaf anzu
  :doc "an Emacs port of anzu.vim"
  :url "https://github.com/syohex/emacs-anzu"
  :ensure t
  :require t
  :config
  (global-anzu-mode 1))


(leaf multiple-cursors
  :url "https://github.com/magnars/multiple-cursors.el"
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/skip-all-like-this)))


(leaf beacon
  :doc ""
  :url "https://github.com/Malabarba/beacon"
  :ensure t
  :custom ((beacon-size . 60)
           (beacon-color . "yellow")
           (beacon-blink-delay . 0.5)
           (beacon-blink-dulation . 0.5))
  :require t
  :config (beacon-mode 1))


(leaf undo-tree
  :doc "visualize undo branches"
  :url "https://www.emacswiki.org/emacs/UndoTree"
  :ensure t
  :require t
  :config
  (global-undo-tree-mode))


(leaf undohist
  :doc "enable undo on closed buffer."
  :url "https://github.com/emacsmirror/undohist"
  :ensure t
  :require t
  :config
  (undohist-initialize))


(leaf doom-themes
  :doc "A theme megapack for GNU Emacs, inspired by community favorites."
  :url "https://github.com/doomemacs/themes"
  :ensure t
  :custom ((doom-themes-enable-bold . t)
           (doom-themes-enable-italic . t))
  :custom-face ((doom-modeline-bar quote ((t (:background "#6272a4")))))
  :require t
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))


(leaf doom-modeline
  :doc "A fancy and fast modeline inspired by minimalism design."
  :url "https://github.com/seagle0128/doom-modeline"
  :ensure t
  :hook (after-init-hook)
  :custom ((doom-modeline-buffer-file-name-style quote truncate-with-project)
           (doom-modeline-icon . t)
           (doom-modeline-major-mode-icon . t)
           (doom-modeline-minor-modes . t))
  :config
  (with-eval-after-load 'doom-modeline
    (line-number-mode 0)
    (column-number-mode 0)
    (doom-modeline-def-modeline 'main
      '(bar window-number matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))))


(leaf neotree
  :doc "A emacs tree plugin like NERD tree for Vim."
  :url "https://github.com/jaypei/emacs-neotree"
  :ensure t
  :defvar neo-persist-show
  :bind (("<f8>" . neotree-toggle))
  :config
  (with-eval-after-load 'neotree
    (setq neo-theme (if (display-graphic-p)
                        'icons 'arrows))
    (setq neo-persist-show t)
    (setq neo-mode-line-type 'none)
    (setq neo-smart-open t)
    (setq neo-window-width 45)
    (add-hook 'neo-after-create-hook
              (lambda (&rest _)
                (display-line-numbers-mode -1)))))


(leaf paren
  :doc "highlight matching parenthesis"
  :ensure nil
  :commands show-paren-mode
  :hook ((after-init-hook . show-paren-mode))
  :custom ((show-paren-style quote mixed)
           (show-paren-when-point-inside-paren . t)
           (show-paren-when-point-in-periphery . t))
  :custom-face ((show-paren-mode quote
                                 ((nil
                                   (:background "#44475a" :foreground "#f1fa8c"))))))


(leaf highlight-indent-guides
  :doc "Indent highlighting."
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :ensure t
  :hook (prog-mode-hook)
  :custom ((highlight-indent-guides-auto-enable . t)
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-character . 124)
           (highlight-indent-guides-method quote character))
  :config
  (with-eval-after-load 'highlight-indent-guides
    (if (fboundp 'diminish)
        (diminish 'highlight-indent-guides-mode))))  ; column


(leaf whitespace
  :doc "indicate whitespaces"
  :ensure t
  :bind (("C-x w" . global-whitespace-mode))
  :custom ((whitespace-style . '(face trailing tabs spaces empty tab-mark space-mark))
           (whitespace-display-mappings . '((space-mark 12288 [9633])
                                            (tab-mark 9 [187 9] [92 9])))
           (whitespace-space-regexp . "\\( +\\|　+\\)"))
  :config
  (with-eval-after-load 'whitespace
    (global-whitespace-mode 1)
    (defvar my/bg-color "#2d3743")
    (set-face-attribute 'whitespace-trailing nil :background my/bg-color :foreground "DeepPink" :underline t)
    (set-face-attribute 'whitespace-tab nil :background my/bg-color :foreground "LightSkyBlue" :underline t)
    (set-face-attribute 'whitespace-space nil :background my/bg-color :foreground "GreenYellow" :weight 'bold)
    (set-face-attribute 'whitespace-empty nil :background my/bg-color)))


(leaf magit
  :doc "an interface to the version control system Git."
  :url "https://github.com/magit/magit"
  :ensure t
  :bind (("C-c C-g" . magit-status)))


(leaf git-gutter
  :doc "show git diff marker"
  :url "https://github.com/emacsorphanage/git-gutter"
  :ensure t
  :custom ((git-gutter:modified-sign . "~")
           (git-gutter:added-sign    . "+")
           (git-gutter:deleted-sign  . "-"))
  :custom-face ((git-gutter:modified . '((t (:background "#f1fa8c"))))
                (git-gutter:added    . '((t (:background "#50fa7b"))))
                (git-gutter:deleted  . '((t (:background "#ff79c6")))))
  :require t
  :config
  (global-git-gutter-mode 1))


(leaf lsp-mode
  :doc ""
  :url "https://github.com/emasc-lsp/lsp-mode"
  :ensure
  :custom
  ;; debug
  (lsp-print-io          . nil)
  (lsp-trace             . nil)
  (lsp-print-performance . nil)
  ;; general
  (lsp-auto-guess-root      . t)
  (lsp-document-sync-method . 'incremental) ;; always send incremental document
  (lsp-response-timeout     . t)
  ;; (lsp-prefer-flymake       . 'flymake)
  ;; completion backend
  (lsp-prefer-capf . t)
  :commands lsp
  :hook ((python-mode-hook . lsp)
	 (lsp-mode-hook . lsp-enable-which-key-integration))
  :bind
  (:lsp-mode-map
   ("C-c r" . lsp-rename))
  :init
  ;; LSP UI tools
  (leaf lsp-ui
    :ensure t
    :after lsp-mode
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable            . t)
    (lsp-ui-doc-header            . t)
    (lsp-ui-doc-include-signature . t)
    (lsp-ui-doc-position          . 'top) ;; top, bottom or at-point
    (lsp-ui-doc-max-width         . 150)
    (lsp-ui-doc-max-height        . 30)
    (lsp-ui-doc-use-childframe    . t)
    (lsp-ui-doc-use-webkit        . t)
    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable           . t)
    (lsp-ui-sideline-ignore-duplicate . t)
    (lsp-ui-sideline-show-symbol      . t)
    (lsp-ui-sideline-show-hover       . t)
    (lsp-ui-sideline-show-diagnostics)
    (lsp-ui-sideline-show-code-actions)
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable        . t)
    (lsp-ui-imenu-kind-position . 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable      . t)
    (lsp-ui-peek-peek-height . 20)
    (lsp-ui-peek-list-width  . 50)
    (lsp-ui-peek-fontify     . 'on-demand) ;; never, on-demand
    :bind
    (:lsp-mode-map
     ("C-c C-r" . lsp-ui-peek-find-references)
     ("C-c C-j" . lsp-ui-peek-find-deninitions)
     ("C-c i"   . lsp-ui-peek-find-implementation)
     ("C-c m"   . lsp-ui-imenu)
     ("C-c s"   . lsp-ui-sideline-mode))
    :hook
    (lsp-mode-hook . lsp-ui-mode)))


(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)


(leaf company-box
  :url "https://github.com/sebastiencs/company-box"
  :ensure t
  :hook (company-mode-hook))


(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))


(leaf ivy
  :doc "Ivy, a generic completion mechanism for Emacs."
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Swiper, an Ivy-enhanced alternative to Isearch."
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))
  (leaf counsel
    :doc "Counsel, a collection of Ivy-enhanced versions of common Emacs commands."
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))


(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)


(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)


(leaf ivy-rich
  :doc "More friendly interface for Ivy"
  :url "https://github.com/Yevgnen/ivy-rich"
  :ensure t
  :after ivy
  :require t
  :config
  (ivy-rich-mode 1))


(leaf all-the-icons-ivy
  :doc "Ivy/Counsel integration for all-the-icons.el"
  :url "https://github.com/asok/all-the-icons-ivy"
  :ensure t
  :require t
  :config
  (all-the-icons-ivy-setup))


(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))


(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)


(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)


(leaf dumb-jump
  :doc "jump to definition for multiple languages without configuration."
  :url "https://github.com/jacktasia/dumb-jump"
  :ensure t
  :bind (([(super d)]       . dumb-jump-go)
         ([(super shift d)] . dumb-jump-back))
  :require t
  :setq ((dumb-jump-mode . t)
         (dumb-jump-selector quote ivy)
         (dumb-jump-use-visible-window)))


(leaf migemo
  :doc "Japanese increment search with 'Romanization of Japanese'(ローマ字)."
  :url "https://github.com/emacs-jp/migemo"
  :when (executable-find "cmigemo")
  :ensure t
  :custom ((migemo-command . "cmigemo")
           (migemo-user-dictionary)
           (migemo-regex-dictionary)
           (migemo-use-pattern-alist . t)
           (migemo-use-frequent-pattern-alist . t)
           (migemo-pattern-alist-length . 1000)
           (migemo-coding-system quote utf-8-unix))
  :config
  (cond
   ((eq system-type 'darwin)
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
   ((eq system-type 'gnu/linux)
    (setq migemo-options '("-q" "--emacs" "-i" ""))
    (setq migemo-dictionary "/usr/share/migemo/migemo-dict")))
  (migemo-init))


;; Programming languages
(leaf python
  :doc "mejor mode for Python"
  :mode "\\.py\\'"
  :interpreter "python")

(leaf auto-virtualenv
  :ensure t
  :hook (python-mode-hook . auto-virtualenv-set-virtualenv))


;; Javascript/Typescript
(leaf js2-mode
  :doc ""
  :url ""
  :ensure t
  :mode ("\\.js\\(_t\\)\\'")
  :require t)

(leaf rjsx-mode
  :doc ""
  :url ""
  :ensure t
  :mode ("\\.jsx\\'")
  :require t)

(leaf typescript-mode
  :doc ""
  :url ""
  :ensure t
  :mode ("\\.ts\\'")
  :require t)


;; Perl
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


;; EmacsLisp
(defun lisp-mode-hooks ()
  "'lisp-mode-hooks'."
  (leaf eldoc
    :ensure t
    :require t
    :setq ((eldoc-idle-delay . 0.2)
           (eldoc-echo-area-use-multiline-p . t))
    :config
    (turn-on-eldoc-mode)))

(add-hook 'emacs-lisp-mode-hook 'lisp-mode-hooks)
(add-hook 'lisp-interaction-mode-hook 'lisp-mode-hooks)
(add-hook 'ielm-mode-hook 'lisp-mode-hooks)


;; Common Lisp
;; Setup load-path, autoloads and your Lisp system.
;; (add-to-list 'load-path "~/.emacs.d/elisp/slime")
;; (use-package slime-autoloads
;;   :config
;;   (setq inferior-lisp-program "sbcl")
;;   (setq slime-net-coding-system 'utf-8-unix)
;;   (slime-setup '(slime-repl slime-fancy slime-banner slime-indentation slime-company))
;;   )


;; HTML templates
(leaf web-mode
  :doc "emacs major mode for editing web templates"
  :url "https://github.com/fxbois/web-mode"
  :ensure t
  :preface
  (defun web-mode-hook nil
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 4))

  :mode ("\\.phtml$" "\\.tpl\\.php$" "\\.jsp$" "\\.as[cp]x$" "\\.erb$" "\\.html?$" "\\.tsx$")
  :hook ((web-mode-hook . web-mode-hook))
  :require t
  :config
  (when (< emacs-major-version 24)
    (defalias 'prog-mode 'fundamental-mode)))


;; Text formats
(leaf markdown-mode
  :doc "emacs major mode for editing Markdown-formatted text"
  :url "https://github.com/jrblevin/markdown-mode"
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         "\\.md\\'" "\\.markdown\\'")
  :setq ((markdown-command . "multimarkdown")))


(leaf rst
  :doc "Emacs support for editing reStructuredText text document."
  :url "https://docutils.sourceforge.io/docs/user/emacs.html"
  :mode ("\\.re?st\\$"))


(leaf yaml
  :doc "Simple major mode to edit YAML file for emacs"
  :url "https://github.com/yoshiki/yaml-mode"
  :emacs>= 24
  :mode ("\\.yam?l$"))


(leaf po-mode
  :doc ""
  :url ""
  :mode ("\\.po//'" "\\.po\\.")
  :emacs>= 22)


(provide 'init)


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; init.el ends here

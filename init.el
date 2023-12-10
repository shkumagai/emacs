;;; init.el --- My init.el -*- lexical-binding: t; -*-
;;; Copyright (C) Shoji KUMAGAI
;;; Commentary:
;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

;; Inithialize package manager for compile time
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  ;; Leaf keywords
  (leaf leaf-keywords
    :doc "Use leaf as a package manager"
    :url "https://github.com/conao3/leaf.el"
    :ensure t
    :init
    (leaf el-get
      :ensure t
      :custom
      (el-get-notify-type       . 'message)
      (el-get-git-shallow-clone . t))
    (leaf hydra :ensure t)
    :config
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert
    :ensure t
    :config (leaf use-package :ensure t))
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

;; Compile
(eval-and-compile
  (leaf *byte-compile
    :custom
    (byte-compile-warnings . '(not free-vars))
    (debug-on-error        . nil)))

(leaf *native-compile
  :doc "Native Compile by gccemacs"
  :url "https://www.emacswiki.org/emacs/GccEmacs"
  :if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  :custom
  (comp-deferred-compilation . nil)
  (comp-speed                . 5)
  (comp-num-cpus             . 4)
  :config
  (native-compile-async "~/.emacs.d/init.el" 4 t)
  (native-compile-async "~/.emacs.d/elpa" 4 t)
  (native-compile-async "~/.emacs.d/el-get" 4 t))

;; --------------------------------------------------------------------------------
;;
;; Generic Configurations
;;
;; --------------------------------------------------------------------------------

(leaf no-littering
  :doc "Help keeping ~/.config/emacs clean"
  :req "emacs-25.1" "compat-29.1.4.2"
  :tag "convenience" "emacs>=25.1"
  :url "https://github.com/emacscollective/no-littering"
  :added "2023-12-08"
  :emacs>= 25.1
  :ensure t
  :custom `((custom-file . ,(no-littering-expand-etc-file-name "custom.el")))
  :require t)

(leaf *to-be-quiet
  :doc "Quiet annoying messages"
  :preface
  (defun display-startup-echo-area-message ()
    "no startup message"
    (message ""))
  :config
  (defalias 'yes-or-no-p #'y-or-n-p))

(leaf *encoding
  :doc "It's time to use UTF-8"
  :config
  (set-locale-environment "en_US.UTF-8")
  (prefer-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (set-selection-coding-system 'utf-8-unix)
  (set-buffer-file-coding-system 'utf-8-unix))

(leaf *formatting
  :custom
  (truncate-lines        . nil)
  (require-final-newline . t)
  (tab-width             . 2)
  (indent-tabs-mode      . nil))

(leaf *autorevert
  :doc "Revert changes if local file is updated"
  :global-minor-mode global-auto-revert-mode
  :custom (auto-revert-interval . 0.1))

(leaf *recovery
  :doc "Save place of cursor"
  :global-minor-mode save-place-mode)

(leaf *savehist
  :doc "Save history of minibuffer"
  :global-minor-mode savehist-mode)

(leaf *recentf
  :doc "Record open files history"
  :global-minor-mode recentf-mode
  :custom
  (recentf-max-saved-items . 20000)
  (recentf-max-menu-items  . 20000)
  (recentf-auto-cleanup    . 'never)
  (recentf-exclude
   . '((expand-file-name package-user-dir)
       ".cache"
       "cache"
       "bookmarks"
       "*.png"
       "*.jpeg"
       "org_archive"
       "COMMIT_MESSAGE\\'")))

;; Basic Editing Operation

(leaf *delsel
  :doc "Replace the active region just by typing text, or delete just hitting the DEL key"
  :global-minor-mode delete-selection-mode)

(leaf undo-fu
  :doc "Undo helper with redo"
  :req "emacs-25.1"
  :tag "emacs>=25.1"
  :url "https://codeberg.org/ideasman42/emacs-undo-fu"
  :added "2023-12-11"
  :emacs>= 25.1
  :ensure t
  :bind*
  ("C-/" . undo-fu-only-undo)
  ("M-/" . undo-fu-only-redo))

;; --------------------------------------------------------------------------------
;;
;; Window System
;;
;; --------------------------------------------------------------------------------

(leaf *adjust-frame-position
  :doc "Place frame on the right side of the screen"
  :if (window-system)
  :config
  (set-frame-position nil (/ (display-pixel-width) 2) 0)
  (if (< (display-pixel-width) 1800)
      (set-frame-size nil 100 63)))

;; Font Size checker
;;
;; |∞≤≥∏∑∫ ×±⊆⊇|
;; |αβγδεζ ηθικλμ|
;; |abcdef ghijkl|
;; |ABCDEF GHIJKL|
;; |'";:-+ =/\~`?|
;; |日本語 の美観|
;; |あいう えおか|
;; |アイウ エオカ|
;; |ｱｲｳｴｵｶ ｷｸｹｺｻｼ|
;;
;; | hoge                 | hogeghoe | age               |
;; |----------------------+----------+-------------------|
;; | 今日もいい天気ですね | お、     | 等幅になった :+1: |

(leaf font-for-gui
  :doc "Adjust font size"
  :if (window-system)
  :preface
  (defun set-jpfonts (family)
    (set-fontset-font t 'japanese-jisx0208 (font-spec :family family :size 16))
    (set-fontset-font t 'japanese-jisx0212 (font-spec :family family :size 16))
    (set-fontset-font t 'jisx0201          (font-spec :family family :size 16))
    (set-fontset-font t 'kana              (font-spec :family family :size 16)))
  (defun set-asciifonts (family)
    (set-fontset-font t 'latin  (font-spec :family family :widthtype "ExtraCondensed"))
    (set-fontset-font t 'greek  (font-spec :family family :widthtype "ExtraCondensed"))
    (set-fontset-font t 'arabic (font-spec :family family :widthtype "ExtraCondensed"))
    (set-fontset-font t 'symbol (font-spec :family family :widthtype "ExtraCondensed")))
  :custom
  (use-default-font-for-symbols   . nil)
  (inhibit-compacting-font-caches . t)
  (asciifont . "Noto Sans Mono")
  (jpfont . "Noto Sans JP")
  :config
  (set-face-attribute 'default nil :family asciifont :height 140)
  (set-jpfonts jpfont)
  (set-asciifonts asciifont))

(leaf mouse
  :if (window-system)
  :custom
  (mouse-wheel-scroll-amount     . '(1 ((shift) . 1)))
  (mouse-wheel-progressive-speed . nil)
  ;; pixel-scroll-mode has a bug around GC, so it stop to use it.
  (scroll-step           . 1)
  (scroll-margin         . 0)
  (scroll-conservatively . 100000))

;; --------------------------------------------------------------------------------
;;
;; MacOS
;;
;; --------------------------------------------------------------------------------

(leaf *pbcopy-and-pbpaste
  :if (equal system-type 'darwin)
  :preface
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (defun paste-to-osx (text)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Message*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  :custom
  (mac-option-modifier         . 'super)
  (mac-command-modifier        . 'meta)
  (interprogram-cut-function   . 'paste-to-osx)
  (interprogram-paste-function . 'copy-from-osx))

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1" "cl-lib-0.6"
  :tag "environment" "unix" "emacs>=24.1"
  :url "https://github.com/purcell/exec-path-from-shell"
  :added "2023-12-08"
  :emacs>= 24.1
  :ensure t
  :custom
  (exec-path-from-shell-check-startup-files . nil)
  (exec-path-from-shell-variables . '("PATH" "GOPATH" "LC_LANG" "LANG"))
  :config
  (exec-path-from-shell-initialize))


;; --------------------------------------------------------------------------------
;;
;; Brackets Guide
;;
;; --------------------------------------------------------------------------------

(leaf smartparens
  :ensure t
  :require smartparens-config
  :global-minor-mode smartparens-global-mode
  :bind
  (:smartparens-mode-map
   ("M-<DEL>" . sp-backward-unwrap-sexp)
   ("M-]"     . sp-up-sexp)
   ("M-["     . sp-down-sexp)
   ("C-("     . sp-beginning-of-sexp)
   ("C-)"     . sp-end-of-sexp)
   ("C-M-f"   . sp-forward-sexp)
   ("C-M-b"   . sp-backward-sexp)
   ("C-M-n"   . sp-next-sexp)
   ("C-M-p"   . sp-previous-sexp))
  :config
  (sp-local-pair 'org-mode "*" "*")
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "~" "~")
  (sp-local-pair 'org-mode "+" "+"))

;; --------------------------------------------------------------------------------
;;
;; Custom Functions
;;
;; --------------------------------------------------------------------------------

;; TBD

;; --------------------------------------------------------------------------------
;;
;; Cursor
;;
;; --------------------------------------------------------------------------------

(leaf *general-cursor-options
  :custom
  (kill-whole-line  . t)
  (track-eol        . t)
  (line-move-visual . nil))

(leaf avy
  :doc "Jump to arbitrary positions in visible text and select text quickly."
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "location" "point" "emacs>=24.1"
  :url "https://github.com/abo-abo/avy"
  :added "2023-12-08"
  :emacs>= 24.1
  :ensure t)

(leaf avy-zap
  :doc "Zap to char using `avy'"
  :req "avy-0.2.0"
  :tag "extensions"
  :url "https://github.com/cute-jumper/avy-zap"
  :added "2023-12-08"
  :ensure t)


;; --------------------------------------------------------------------------------
;;
;; Window Layout
;;
;; --------------------------------------------------------------------------------

(leaf ace-window
  :doc "Quickly switch windows."
  :req "avy-0.5.0"
  :tag "location" "window"
  :url "https://github.com/abo-abo/ace-window"
  :added "2023-12-08"
  :ensure t
  :bind
  ("C-o" . ace-window)
  :custom-face
  (aw-leading-char-face . '((t (:height 4.0 :foregroud "#f1fa8c")))))

(leaf rotate
  :doc "Rotate the layout of emacs"
  :tag "layout" "window"
  :url "https://github.com/daichirata/emacs-rotate"
  :added "2023-12-11"
  :ensure t)

(leaf *window-maximizer
  :doc "Maximize current window"
  :if (window-system)
  :custom
  (is-window-maximized . nil)
  :preface
  (defun toggle-window-maximize ()
    (interactive)
    (progn
      (if is-window-maxmized
          (balance-windows)
        (maximize-window))
      (setq is-window-maximized
            (not is-window-maximized)))))

(leaf *window-transparency
  :doc "Set window transparecy level"
  :if (window-system)
  :hook (after-init-hook . toggle-window-transparency)
  :custom
  (window-transparency . 88)
  :preface
  (defun toggle-window-transparency ()
    "Cycle the frame transparency from default to transparent."
    (interactive)
    (let ((transparency window-transparency)
          (opacity 100))
      (if (and (not (eq (frame-parameter nil 'alpha) nil))
               (< (frame-parameter nil 'alpha) opacity))
          (set-frame-parameter nil 'alpha opacity)
        (set-frame-parameter nil 'alpha transparency)))))

;; --------------------------------------------------------------------------------
;;
;; Error Checker
;;
;; --------------------------------------------------------------------------------

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "emacs-25.1" "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11"
  :tag "tools" "languages" "convenience" "emacs>=25.1"
  :url "http://www.flycheck.org"
  :added "2023-12-08"
  :emacs>= 25.1
  :ensure t
  :global-minor-mode global-flycheck-mode
  :custom
  (flycheck-dispaly-errors-delay . 0))

;; --------------------------------------------------------------------------------
;;
;; Completion
;;
;; --------------------------------------------------------------------------------

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-25.1"
  :tag "matching" "convenience" "abbrev" "emacs>=25.1"
  :url "http://company-mode.github.io/"
  :added "2023-12-08"
  :emacs>= 25.1
  :ensure t
  :hook (prog-mode-hook . company-mode)
  :bind
  ((:company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("<tab>" . company-complete-common-or-cycle))
   (:company-search-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)))
  :custom
  (company-idle-delay  . 0) ; 補完の遅延なし
  (company-echo-delay  . 0)
  (company-ignore-case . t)
  (company-selection-wrap-around . t)
  (company-minimum-prefix-length . 1) ; 1文字から補完開始
  :custom-face
  (company-tooltip          . '((t (:background "#323445"))))
  (company-template-field   . '((t (:background "#ff79c6")))))

;; --------------------------------------------------------------------------------
;;
;; Tools
;;
;; --------------------------------------------------------------------------------

;; Docker -------------------------------------------------------------------------

;; (leaf docker
;;   :doc "Interface to Docker"
;;   :req "aio-1.0" "dash-2.19.1" "emacs-26.1" "s-1.13.0" "tablist-1.1" "transient-0.4.3"
;;   :tag "convenience" "filename" "emacs>=26.1"
;;   :url "https://github.com/Silex/docker.el"
;;   :added "2023-12-08"
;;   :emacs>= 26.1
;;   :ensure t
;;   :after aio tablist)

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

;; Git ----------------------------------------------------------------------------

(leaf *git-commit-mode
  :doc "Mode for git commit message editing"
  :mode "\\COMMIT_EDITMSG?")

(leaf git-modes
  :doc "Major modes for editing Git configuration files"
  :req "emacs-25.1" "compat-29.1.4.1"
  :tag "git" "vc" "convenience" "emacs>=25.1"
  :url "https://github.com/magit/git-modes"
  :added "2023-12-08"
  :emacs>= 25.1
  :ensure t)

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "compat-29.1.4.4" "dash-20221013" "git-commit-20231030" "magit-section-20231202" "seq-2.24" "transient-20231204" "with-editor-20230917"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :url "https://github.com/magit/magit"
  :added "2023-12-07"
  :emacs>= 25.1
  :ensure t
  :bind (("C-c C-g" . magit-status)))

(leaf git-gutter
  :doc "Port of Sublime Text plugin GitGutter"
  :req "emacs-25.1"
  :tag "emacs>=25.1"
  :url "https://github.com/emacsorphanage/git-gutter"
  :added "2023-12-09"
  :emacs>= 25.1
  :ensure t
  :global-minor-mode global-git-gutter-mode
  :custom
  (git-gutter:modified-sign . "~")
  (git-gutter:added-sign    . "+")
  (git-gutter:deleted-sign  . "-")
  :custom-face
  (git-gutter:modified . '((t (:background "#f1fa8c"))))
  (git-gutter:added    . '((t (:background "#50fa7b"))))
  (git-gutter:deleted  . '((t (:background "#ff79c6")))))

;; --------------------------------------------------------------------------------
;;
;; Programming Mode
;;
;; --------------------------------------------------------------------------------

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-27.1" "dash-2.18.0" "f-0.20.0" "ht-2.3" "spinner-1.7.3" "markdown-mode-2.3" "lv-0" "eldoc-1.11"
  :tag "languages" "emacs>=27.1"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :added "2023-12-08"
  :emacs>= 27.1
  :ensure t
  :custom
  ;; General settings
  (lsp-auto-guess-root              . t)
  (lsp-modline-diagnostics-enable   . t)
  (lsp-headerline-breadcrumb-enable . nil)
  ;; Performance tuning
  (lsp-log-io               . nil)
  (lsp-print-performance    . nil)
  (lsp-completion-provider  . :none)
  (lsp-enable-file-watchers . nil)
  (lsp-idle-delay           . 0.500)
  (gc-cons-threshold        . 100000000)
  (read-process-output-max  . 1048576)
  :bind
  (:lsp-mode-map
   ("C-c r"   . lsp-rename)
   ("C-c C-c" . lsp-execute-code-action))
  :hook
  (lsp-mode-hook
   . (lambda ()
       (setq-local company-backends '((company-yasnippet company-capf :separate))))))

(leaf lsp-ui
  :doc "UI modules for lsp-mode"
  :req "emacs-27.1" "dash-2.18.0" "lsp-mode-6.0" "markdown-mode-2.3"
  :tag "tools" "languages" "emacs>=27.1"
  :url "https://github.com/emacs-lsp/lsp-ui"
  :added "2023-12-08"
  :emacs>= 27.1
  :ensure t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :custom
  (lsp-ui-flycheck-enable     . t)
  (lsp-ui-sideline-enable     . t)
  (lsp-ui-sideline-show-hover . nil)
  (lsp-ui-imenu-enable        . nil)
  (lsp-ui-peek-fontify        . 'on-demand)
  (lsp-ui-peek-enable         . t)
  (lsp-ui-doc-enable          . nil)
  (lsp-ui-doc-max-height      . 12)
  (lsp-ui-doc-max-width       . 56)
  (lsp-ui-doc-position        . 'at-point)
  (lsp-ui-doc-border          . "#323445")
  :custom-face
  (lsp-ui-doc-background . '((t (:background "#282a36"))))
  (lsp-ui-doc-header     . '((t (:background "#76e0f3" :weight bold))))
  (lsp-ui-doc-url        . '((t (:background "#6272a4"))))
  :bind
  ((:lsp-mode-map
    ("C-c C-r"   . lsp-ui-peek-find-references)
    ("C-c C-j"   . lsp-ui-peek-find-definitions)
    ("C-c C-M-j" . xref-find-definitions-other-window)
    ("C-c i"     . lsp-ui-peek-find-implementation)
    ("C-c m"     . counsel-imenu)
    ("C-c M"     . lsp-ui-imenu)
    ("C-c s"     . toggle-lsp-ui-sideline)
    ("C-c d"     . toggle-lsp-ui-doc))
   (:lsp-ui-doc-mode-map
    ("q"         . toggle-lsp-ui-doc)
    ("C-i"       . lsp-ui-doc-focus-frame)))
  :init
  (defun toggle-lsp-ui-sideline ()
    (interactive)
    (if lsp-ui-sideline-show-hover
        (progn
          (setq lsp-ui-sideline-show-hover nil)
          (message "sideline-hover disabled :P"))
      (progn
        (setq lsp-ui-sideline-show-hover t)
        (message "sideline-hover enabled :)"))))
  (defun toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame)
          (message "lsp-ui-doc disabled :P"))
      (progn
        (lsp-ui-doc-mode 1)
        (message "lsp-ui-doc enabled :)")))))

;; Python -------------------------------------------------------------------------

(leaf python
  :doc "Python's flying circus support for Emacs"
  :tag "builtin"
  :added "2023-12-08"
  :mode ("\\.py\\'" . python-mode)
  :hook
  (python-mode-hook
   . (lambda ()
       (lsp-deferred)
       (add-hook 'before-save-hook #'lsp-organize-imports t t))))

(leaf lsp-pyright
  :doc "Python LSP client using Pyright"
  :req "emacs-26.1" "lsp-mode-7.0" "dash-2.18.0" "ht-2.0"
  :tag "lsp" "tools" "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-pyright"
  :added "2023-12-08"
  :emacs>= 26.1
  :ensure t
  :after lsp-mode
  :custom
  (python-shell-interpreter          . "python3")
  (lsp-pyright-python-executable-cmd . "python3")
  :require lsp-pyright)

;; JavaScript/TypeScript ----------------------------------------------------------

(leaf js2-mode
  :doc "Improved JavaScript editing mode"
  :req "emacs-24.1" "cl-lib-0.5"
  :tag "javascript" "languages" "emacs>=24.1"
  :url "https://github.com/mooz/js2-mode/"
  :added "2023-12-08"
  :emacs>= 24.1
  :ensure t
  :mode "\\.js\\'")

(leaf typescript-mode
  :doc "Major mode for editing typescript"
  :req "emacs-24.3"
  :tag "languages" "typescript" "emacs>=24.3"
  :url "http://github.com/ananthakumaran/typescript.el"
  :added "2023-12-08"
  :emacs>= 24.3
  :ensure t
  :mode "\\.ts\\'")

;; --------------------------------------------------------------------------------
;;
;; Configuration Languages
;;
;; --------------------------------------------------------------------------------

;; YAML ---------------------------------------------------------------------------

(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :req "emacs-24.1"
  :tag "yaml" "data" "emacs>=24.1"
  :url "https://github.com/yoshiki/yaml-mode"
  :added "2023-12-08"
  :emacs>= 24.1
  :ensure t
  :mode "\\.ya?ml?"
  :defvar yaml-file-threshold
  :custom
  (yaml-file-threshold . 100000))

;; Dockerfile ---------------------------------------------------------------------

(leaf dockerfile-mode
  :doc "Major mode for editing Docker's Dockerfiles"
  :req "emacs-24"
  :tag "tools" "processes" "languages" "docker" "emacs>=24"
  :url "https://github.com/spotify/dockerfile-mode"
  :added "2023-12-08"
  :emacs>= 24
  :ensure t
  :mode "\\Dockerfile\\'")

;; Terraform ----------------------------------------------------------------------

(leaf terraform-mode
  :doc "Major mode for terraform configuration file"
  :req "emacs-24.3" "hcl-mode-0.3" "dash-2.17.0"
  :tag "emacs>=24.3"
  :url "https://github.com/syohex/emacs-terraform-mode"
  :added "2023-12-08"
  :emacs>= 24.3
  :ensure t
  :after hcl-mode
  :mode "\\.tf\\'")

;; Markdown -----------------------------------------------------------------------

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text"
  :req "emacs-27.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=27.1"
  :url "https://jblevins.org/projects/markdown-mode/"
  :added "2023-12-08"
  :emacs>= 27.1
  :ensure t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode))
  :bind
  ((:markdown-mode-map
    ("M-t u" . markdown-toggle-url-hiding)
    ("M-t m" . markfown-toggle-markup-hidng)))
  :custom
  (markdown-hide-urls         . nil)
  (markdown-hide-markup       . nil)
  (markdown-list-item-bullets . '("・"))
  (markdown-fontify-code-blocks-natively . t)
  :custom-face
  (markdown-header-face-1         . '((t (:inherit outline-1 :weight bold   :height 1.5))))
  (markdown-header-face-2         . '((t (:inherit outline-1 :weight normal :height 1.2))))
  (markdown-header-face-3         . '((t (:inherit outline-1 :weight normal :height 1.1))))
  (markdown-header-face-4         . '((t (:inherit outline-1 :weight normal))))
  (markdown-bold-face             . '((t (:foreground "#f8f8f2" :weight bold))))
  (markdown-italic-face           . '((t (:foreground "#f8f8f2" :slant italic))))
  (markdown-header-delimiter-face . '((t (:foreground "#6272a4" :weight normal))))
  (markdown-link-face             . '((t (:foreground "#f1fa8c"))))
  (markdown-url-face              . '((t (:foreground "#6272a4"))))
  (markdown-list-face             . '((t (:foreground "#6272a4"))))
  (markdown-gfm-checkbox-face     . '((t (:foreground "#6272a4"))))
  (markdown-metadata-value-face   . '((t (:foreground "#8995ba"))))
  (markdown-metadata-key-face     . '((t (:foreground "#6272a4"))))
  (markdown-pre-face              . '((t (:foreground "#8be9fd")))))


;; --------------------------------------------------------------------------------
;;
;; Theme
;;
;; --------------------------------------------------------------------------------

(leaf doom-themes
  :doc "an opinionated pack of modern color-themes"
  :req "emacs-25.1" "cl-lib-0.5"
  :tag "faces" "themes" "emacs>=25.1"
  :url "https://github.com/doomemacs/themes"
  :added "2023-12-07"
  :emacs>= 25.1
  :ensure t
  :defer-config
  (let ((display-table (or standard-display-table (make-display-table))))
    (set-display-table-slot display-table 'vertical-border (make-glyph-code ?|))
    (setq standard-display-table display-table))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(leaf doom-theme-for-term
  :doc "Show repository root in NetTree"
  :unless (window-system)
  :preface
  (defun doom-themes-neotree-insert-root-for-term (node)
    ;; insert icon and project name
    (insert
     (propertize
      (concat (propertize "" 'face 'net-root-dir-face)
              (or (neo-path--file-short-name node) "-")
              "\n")
      'face `(:inherit ,(append (if doom-themes-neotree-enable-variable-pitch '(variable-pitch))
                                '(neo-root-dir-face))))))
  :advice
  (:override doom-themes-neotree-insert-root doom-themes-neotree-insert-root-for-term))

(leaf nano-modeline
  :doc "N Λ N O modeline"
  :req "emacs-27.1"
  :tag "header-line" "mode-line" "convenience" "emacs>=27.1"
  :url "https://github.com/rougier/nano-modeline"
  :added "2023-12-09"
  :emacs>= 27.1
  :ensure t
  :custom
  (frame-background-mode . 'dark)
  (nano-color-foreground . "#f8f8f2")
  (nano-color-background . "#282a36")
  (nano-color-highlight  . "#373844")
  (nano-color-critical   . "#bd93f9")
  (nano-color-salient    . "#0189cc")
  (nano-color-strong     . "#e2e2dc")
  (nano-color-popout     . "#f8f8f2")
  (nano-color-subtle     . "#44475a")
  (nano-color-faded      . "#6272a4")
  :custom-face
  (hl-line                   . '((t (:background "#3B4252" :extend t))))
  (vertical-border           . '((t (:background "#282A36" :foreground "#1E2029"))))
  (mode-line                 . '((t (:background "#282A36"))))
  (mode-line-inactive        . '((t (:background "#282A36"))))
  (nano-face-header-salient  . '((t (:foreground "#282A36" :background "#0189CC"))))
  (nano-face-header-popout   . '((t (:foreground "#282A36" :background "#F1FA8C"))))
  (nano-face-header-critical . '((t (:foreground "#282A36" :background "#BD93F9"))))
  (nano-face-header-faded    . '((t (:foreground "#282A36" :background "#6272A4"))))
  (nano-face-subtle          . '((t (:foreground "#282A36" :background "#44475A"))))
  (nano-face-header-default  . '((t (:foreground "#b0b8d1" :background "#44475A"))))
  (nano-face-header-strong   . '((t (:foreground "#f8f8f2" :background "#44475A" :weight bold)))))


;; --------------------------------------------------------------------------------
;;
;; Widgets
;;
;; --------------------------------------------------------------------------------

(leaf all-the-icons
  :doc "A library for inserting Developer icons"
  :req "emacs-24.3"
  :tag "lisp" "convenient" "emacs>=24.3"
  :url "https://github.com/domtronn/all-the-icons.el"
  :added "2023-12-09"
  :emacs>= 24.3
  :ensure t)

(leaf neotree
  :doc "A tree plugin like NerdTree for Vim"
  :req "cl-lib-0.5"
  :url "https://github.com/jaypei/emacs-neotree"
  :added "2023-12-09"
  :ensure t
  :bind
  ("<f9>" . neotree-projectile-toggle)
  :custom
  (neo-theme             . 'nerd)
  (neo-cwd-line-style    . 'button)
  (neo-autorefresh       . t)
  (neo-show-hidden-files . t)
  (neo-mode-line-type    . nil)
  (neo-window-fixed-size . nil)
  :hook (neotree-mode-hook . neo-hide-nano-header)
  :preface
  (defun neo-hide-nano-header ()
    "Hide nano header"
    (interactive)
    (setq header-line-format ""))
  (defun neotree-projectile-toggle ()
    "Toggle function for projectile."
    (interactive)
    (let ((project-dir
           (ignore-errors
             (projectile-project-root)))
          (file-name (buffer-file-name)))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name))))))
  :config
  ;; Use nerd font in terminal.
  (unless (window-system)
    (advice-add
     'neo-buffer--insert-fold-symbol
     :override
     (lambda (name &optional node-name)
       (let ((n-insert-symbol (lambda (n)
                                (neo-buffer--insert-with-face
                                 n 'neo-expand-btn-face))))
         (or (and (equal name 'open)  (funcall n-insert-symbol ""))
             (and (equal name 'close) (funcall n-insert-symbol ""))
             (and (equal name 'leaf)  (funcall n-insert-symbol ""))))))))


;; --------------------------------------------------------------------------------
;;
;; Accessibility
;;
;; --------------------------------------------------------------------------------

;; Input Assistance
(leaf *hydra-theme
  :doc "Make emacs bindings that stick around"
  :url "https://github.com/abo-abo/hydra"
  :custom-face
  (hydra-face-red      . '((t (:foreground "#bd93f9"))))
  (hydra-face-blue     . '((t (:foreground "#8be9fd"))))
  (hydra-face-pink     . '((t (:foreground "#ff79c6"))))
  (hydra-face-teal     . '((t (:foreground "#61bfff"))))
  (hydra-face-amaranth . '((t (:foreground "#f1fa8c")))))

(leaf major-mode-hydra
  :doc "Major mode keybindings managed by Hydra"
  :req "dash-2.18.0" "pretty-hydra-0.2.2" "emacs-25"
  :tag "emacs>=25"
  :url "https://github.com/jerrypnz/major-mode-hydra.el"
  :added "2023-12-11"
  :emacs>= 25
  :ensure t
  :require pretty-hydra)

(leaf hydra-posframe
  :doc "Show hidra hints on posframe"
  :url "https://github.com/Ladicle/hydra-posframe"
  :if (window-system)
  :el-get "Ladicle/hydra-posframe"
  :global-minor-mode hydra-posframe-mode
  :custom
  (hydra-posframe-border-width . 5)
  (hydra-posframe-parameters   . '((left-fringe . 8) (right-fringe . 8)))
  :custom-face
  (hydra-posframe-border-face . '((t (:background "#323445")))))

(leaf which-key
  :doc "Display available keybindings in popup"
  :req "emacs-24.4"
  :tag "emacs>=24.4"
  :url "https://github.com/justbur/emacs-which-key"
  :added "2023-12-07"
  :emacs>= 24.4
  :ensure t
  :global-minor-mode which-key-mode)

(leaf amx
  :doc "Alternative M-x with extra features."
  :req "emacs-24.4" "s-0"
  :tag "completion" "usability" "convenience" "emacs>=24.4"
  :url "http://github.com/DarwinAwardWinner/amx/"
  :added "2023-12-07"
  :emacs>= 24.4
  :ensure t
  :hook (after-init-hook))

(leaf editorconfig
  :doc "EditorConfig Emacs Plugin"
  :req "emacs-26.1" "nadvice-0.3"
  :tag "editorconfig" "convenience" "emacs>=26.1"
  :url "https://github.com/editorconfig/editorconfig-emacs#readme"
  :added "2023-12-07"
  :emacs>= 26.1
  :ensure t)

(leaf display-line-numbers
  :doc "Display line number"
  :req "emacs-26.0.5"
  :tag "builtin" "emacs>=26.0.5"
  :url "https://www.emacssiki.org/emacs/LineNumbers"
  :hook ((python-mode-hook terraform-mode-hook) . display-line-numbers-mode))

(leaf display-fill-column-indicator
  :doc "Indicate maximum column"
  :req "emacs-27.0"
  :tag "builtin"
  :url "https://www.emacswiki.org/emacs/FillColumnIndicator"
  :hook ((markdown-mode-hook, git-commit-mode-hook) . display-fill-column-indicator-mode))


;; --------------------------------------------------------------------------------
;;
;; Highligiting
;;
;; --------------------------------------------------------------------------------

(leaf paren
  :doc "Highlight paired parens and brackets"
  :url "https://www.emacswiki.org/emacs/ShowParenMode"
  :global-minor-mode show-paren-mode
  :custom
  (show-paren-style . 'mixed)
  (show-paren-when-point-inside-paren . t)
  (show-paren-when-point-in-periphery . t)
  :custom-face
  (show-paren-match . '((t (:background "#44475a" :foreground "#f1fa8c")))))

(leaf whitespace
  :doc "minor mode to visualize TAB, (HARD) SPACE, NEWLINE"
  :req "emacs-24.4"
  :tag "builtin"
  :added "2023-12-07"
  :ensure t
  :bind (("C-c w" . global-whitespace-mode))
  :custom ((whitespace-style . '(face trailing tabs spaces empty tab-mark space-mark))
           (whitespace-display mappings . '((space-mark 12288 [9633])
                                            (tab-mark 9 [187 9] [92 9])))
           (whitespace-space-regexp . "\\( +\\|+\\)"))
  :config
  (with-eval-after-load 'whitespace
    (global-whitespace-mode 1)
    (defvar my/bg-color "#2d3743")
    (set-face-attribute 'whitespace-trailing nil :background my/bg-color :foreground "DeepPink"     :underline t)
    (set-face-attribute 'whitespace-tab      nil :background my/bg-color :foreground "LightSyeBlue" :underline t)
    (set-face-attribute 'whitespace-space    nil :background my/bg-color :foreground "GreenYellow"  :underline t)
    (set-face-attribute 'whitespace-empty    nil :background my/bg-color)))

(leaf highlight-indent-guides
  :doc "Minor mode to highlight indentation"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :added "2023-12-09"
  :emacs>= 24.1
  :ensure t
  :hook (prog-mode-hook . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled . t)
  (highlight-indent-guides-responsive   . t)
  (highlight-indent-guides-method . 'bitmap)
  :config
  (highlight-indent-guides-auto-set-faces))

(leaf beacon
  :doc "Highlight the cursor whenever the window scrolls"
  :req "emacs-25.1"
  :tag "convenience" "emacs>=25.1"
  :url "https://github.com/Malabarba/beacon"
  :added "2023-12-09"
  :emacs>= 25.1
  :ensure t
  :custom (beacon-color . "#f1fa8c"))


;; --------------------------------------------------------------------------------
;;
;; Search Interface
;;
;; --------------------------------------------------------------------------------

(leaf anzu
  :doc "Show number of matches in mode-line while searching"
  :req "emacs-25.1"
  :tag "emacs>=25.1"
  :url "https://github.com/emacsorphanage/anzu"
  :added "2023-12-09"
  :emacs>= 25.1
  :ensure t
  :config
  (global-anzu-mode 1))

(leaf projectile
  :doc "Manage and navigate projects in Emacs easily"
  :req "emacs-25.1"
  :tag "convenience" "project" "emacs>=25.1"
  :url "https://github.com/bbatsov/projectile"
  :added "2023-12-09"
  :emacs>= 25.1
  :ensure t
  :global-minor-mode projectile-mode)

;; Vertico ------------------------------------------------------------------------

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :req "emacs-27.1" "compat-29.1.4.4"
  :tag "completion" "matching" "files" "convenience" "emacs>=27.1"
  :url "https://github.com/minad/vertico"
  :added "2023-12-10"
  :emacs>= 27.1
  :ensure t
  :global-minor-mode vertico-mode
  :custom
  (vertico-cycle . t)
  (vertico-count . 18))

(leaf vertico-posframe
  :doc "Using posframe to show Vertico"
  :req "emacs-26.0" "posframe-1.4.0" "vertico-1.1"
  :tag "vertico" "matching" "convenience" "abbrev" "emacs>=26.0"
  :url "https://github.com/tumashu/vertico-posframe"
  :added "2023-12-10"
  :emacs>= 26.0
  :ensure t
  :after posframe vertico
  :global-minor-mode vertico-posframe-mode
  :custom
  (vertico-posframe-border-width . 5)
  (vertico-posframe-parameters
   . '((left-fringe . 8)
       (right-fringe . 8)))
  :custom-face
  (vertico-posframe-border . '((t (:background "#323445")))))

(leaf consult
  :doc "Consulting completing-read"
  :req "emacs-27.1" "compat-29.1.4.1"
  :tag "completion" "files" "matching" "emacs>=27.1"
  :url "https://github.com/minad/consult"
  :added "2023-12-10"
  :emacs>= 27.1
  :ensure t
  :bind
  ("M-y"   . consult-yank-pop)
  ("C-M-s" . consult-line)
  :custom (consult-async-min-input . 1))

(leaf consult-flycheck
  :doc "Provides the command `consult-flycheck'"
  :req "emacs-27.1" "consult-1.0" "flycheck-32"
  :tag "completion" "tools" "languages" "emacs>=27.1"
  :url "https://github.com/minad/consult"
  :added "2023-12-10"
  :emacs>= 27.1
  :ensure t
  :after consult flycheck)

(leaf affe
  :doc "Asynchronous Fuzzy Finder for Emacs"
  :req "emacs-27.1" "consult-1.0"
  :tag "completion" "files" "matching" "emacs>=27.1"
  :url "https://github.com/minad/affe"
  :added "2023-12-10"
  :emacs>= 27.1
  :ensure t
  :after consult)

(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :req "emacs-27.1" "compat-29.1.4.0"
  :tag "completion" "matching" "help" "docs" "emacs>=27.1"
  :url "https://github.com/minad/marginalia"
  :added "2023-12-10"
  :emacs>= 27.1
  :ensure t
  :global-minor-mode marginalia-mode
  :custom-face
  (marginalia-documentaion . '((t (:foreground "#6272a4")))))

(leaf orderless
  :doc "Completion style for matching regexps in any order"
  :req "emacs-26.1"
  :tag "extensions" "emacs>=26.1"
  :url "https://github.com/oantolin/orderless"
  :added "2023-12-10"
  :emacs>= 26.1
  :ensure t
  :preface
  (defun flex-if-apostrophe (pattern _index _total)
    (when (string-suffix-p "'" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))
  :custom
  (completion-styles           . '(orderless))
  (orderless-style-dispatchers . '(flex-if-apostrophe
                                   without-if-bang)))

(leaf embark
  :doc "Conveniently act on minibuffer completions"
  :req "emacs-27.1" "compat-29.1.4.0"
  :tag "convenience" "emacs>=27.1"
  :url "https://github.com/oantolin/embark"
  :added "2023-12-10"
  :emacs>= 27.1
  :ensure t
  :bind*
  ("M-a" . embark-act)
  :custom
  (prefix-help-command . #'embark-prefix-help-command)
  :config
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(leaf embark-consult
  :doc "Consult integration for Embark"
  :req "emacs-27.1" "compat-29.1.4.0" "embark-1.0" "consult-1.0"
  :tag "convenience" "emacs>=27.1"
  :url "https://github.com/oantolin/embark"
  :added "2023-12-10"
  :emacs>= 27.1
  :ensure t
  :after embark consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; --------------------------------------------------------------------------------
;;
;; Global Bindings
;;
;; --------------------------------------------------------------------------------

;; Hydra Templates ----------------------------------------------------------------

(leaf *hydra-goto
  :doc "Search and move cursor"
  :bind ("M-j" . *hydra-goto/body)
  :pretty-hydra
  ((:title "Goto" :color blue :quit-key "q" :foreign-keys warn :separator"-")
   ("Goto"
    (("i" avy-goto-char       "char")
     ("t" avy-goto-char-timer "timer")
     ("w" avy-goto-word-2     "word")
     ("j" avy-resume          "resume"))
    "Line"
    (("h" avy-goto-line        "head")
     ("e" avy-goto-end-of-line "end")
     ("n" consult-goto-line    "number"))
    "Topic"
    (("o"  consult-outline      "outline")
     ("m"  consult-imenu        "imenu")
     ("gm" consult-global-imenu "global imenu"))
    "Error"
    (("," flycheck-previous-error "previous" :exit nil)
     ("." flycheck-next-error     "next"     :exit nil)
     ("l" consult-flycheck        "list")))))

(leaf *hydra-toggle
  :doc "Toggle functions"
  :bind ("M-t" . *hydra-toggle/body)
  :pretty-hydra
  ((:title "Toggle" :color blue :quit-key "q" :foreign-keys warn :separator "-")
   ("Basic"
    (("v" view-mode          "view mode"  :toggle t)
     ("w" whitespace-mode    "whitespace" :toggle t)
     ("W" whitespace-cleanup "whitespace cleanup")
     ("b" beacon-mode        "beacon"     :toggle t))
    "Line & Column"
    (("l" toggle-truncate-lines "truncate line" :toggle t)
     ("n" display-line-numbers-mode "line number" :toggle t)
     ("f" display-fill-column-indicator-mode "column indicator" :toggle t))
    "Highlight"
    (("i" highlight-indent-guides-mode "indent guide" :toggle t)))))


;;; Behaviors
;; remove trailing whitespace on save
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ;; Key bindings
;; (bind-key "C-x C-h" 'help global-map)
;; (bind-key "C-h" 'describe-bindings global-map)

;; put here

(provide 'init)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(blackout el-get hydra leaf-keywords leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here

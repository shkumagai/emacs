;; early-init.el --- Early initialization file for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Emacs Setup File

;;; =========================================================================
;;; early-init.el は Emacs 27.1 以降で導入された、Emacs の起動プロセスの早い段階で
;;; 読み込まれる設定ファイル。これにより、init.el よりも前に特定の設定を適用することができる。
;;;
;;; [early-init.el の目的]
;;; * 起動速度の向上: パッケージ初期化やGC設定の最適化
;;; * GUIの安定化: フレーム生成前に見た目を設定してチラつきを減らす
;;; * 設定ファイルの整理: early-init.el にはフレームや起動前に必要な設定を記述。
;;;   それ以外は init.el に分離
;;;
;;; 書くべきでないものは色々あるが、early-init.el に書けるものを書くのではなく、
;;; early-init.el に書いて意味のあるものを書く、という運用。
;;; =========================================================================

;;; Code:

;;; =========================================================================
;;; パッケージ管理
;;; =========================================================================

;; ------------------------------------------------------------
;; パッケージシステムの初期化を抑制
;; 手動管理することで起動時間を短縮する
;; ------------------------------------------------------------
(setq package-enable-at-startup nil)

;; ------------------------------------------------------------
;; パッケージのバイトコンパイル
;; ------------------------------------------------------------
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

;; ------------------------------------------------------------
;; パッケージ管理設定
;; ------------------------------------------------------------
;; パッケージのインストール先
(setq package-user-dir (expand-file-name "var/elpa/" user-emacs-directory))
;; ネイティブコンパイルキャッシュ（eln-cache）
(when (boundp 'native-comp-eln-load-path)
    (startup-redirect-eln-cache (expand-file-name "var/eln-cache/" user-emacs-directory)))
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")
                       ("nongnu"   . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))
  (leaf leaf-keywords
    :ensure t
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))


;;; =========================================================================
;;; 設定諸々
;;; =========================================================================
(leaf *early-init*
  :config

  (leaf *起動速度の向上========================================================
    :config

    (leaf *起動時のGC抑制------------------------------------------------------
      :doc "まずは最大に"
      :custom (gc-cons-threshold . most-positive-fixnum)
      :config
      (leaf *起動後にGCを戻す
        :doc "デフォルトは800000。最後に戻す"
        :doc "hookだとうまく動かないので、configで対応する"
        :config (add-hook 'emacs-startup-hook
                          ;; デフォルト(800000)はLSPの大量データ処理でGCが頻発するため
                          ;; 64MBに設定する
                          (lambda () (setq gc-cons-threshold (* 64 1024 1024)))))
      )
    (leaf *メニューバーやツールバーを表示しない-----------------------------------
      :doc "(menu-bar-mode -1)や(tool-bar-mode -1)では速度的に意味はない"
      :push ((default-frame-alist . '(menu-bar-lines . 0))
             (default-frame-alist . '(tool-bar-lines . 0)))
      )
    (leaf *スタートアップメッセージの非表示--------------------------------------
      :doc "early-initでやっているケースが多いのでそれに倣う"
      :custom (inhibit-startup-message . t))
    )

  (leaf *GUI周り設定==========================================================
    :config

    (leaf *新規フレームに対してフォント設定--------------------------------------
      :url "https://apribase.net/2024/07/06/emacs-default-frame-alist/"
      :doc "↑のURLを参考に設定。early-initなので再読み込みはしない前提でadd-to-listではなくpushしている"
      :push ((default-frame-alist . '(font . "NotoSansM Nerd Font Mono"))))

    (leaf *行番号を表示する----------------------------------------------------
      :url "https://www.grugrut.net/posts/201910202227/"
      :doc "display-line-numbers-width-startは、skk使ってたときの名残り"
      :custom ((global-display-line-numbers-mode . t)
               (custom-set-variables . '(display-line-numbers-width-start t))))

    (leaf *列番号を表示する----------------------------------------------------
      :config (column-number-mode 1))
    )

  (leaf *その他フレーム起動前にやりたい設定=======================================
    :config

    (leaf *バイトコンパイルの警告の非表示----------------------------------------
      :doc "Package xx is deprecated 等を抑制する"
      :doc "解決策がなくて鬱陶しくなったらOnにするが、極力自力で潰し込む"
      :ignored
      :custom (byte-compile-warnings . '(cl-functions))))

  ;; (leaf *その他init.elでエラーが起きても設定されていないと困る最低限設定=============
  ;;   :config
  ;;   ;; TBD
  ;;   )

  ) ; end of early-init


(provide 'early-init)

;;; early-init.el ends here

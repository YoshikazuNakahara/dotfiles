;; ------------------------------------------------------------------------
;; @ 一般設定

;;; デバッグモードでの起動
(require 'cl)

;; 質問をy/nで回答する
(fset 'yes-or-no-p 'y-or-n-p)

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")
;;
;; パッケージ管理(package.el)の設定
;;
(require 'package)

;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; MELPA-stableを追加
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; init.el で package-install() せず、M-x package-list-packages から
;; インストールする場合、これらは不要。package-install() が良しなに
;; 初期化してくれるため。
(package-initialize)
(package-refresh-contents)

;; すでにインストール済みかを package-installed-p() でチェックする
;; 必要はない。package-install() から呼ばれる
;; package-compute-transaction() でチェックしているため。
(package-install 'flycheck)
(package-install 'migemo)
(package-install 'company)
(package-install 'ddskk)
(package-install 'bind-key)
(package-install 'use-package)
;; ロックファイル / バックアップファイルを作成しない
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq delete-auto-save-files t)

;; 自動保存ファイルを作成しない
(setq auto-save-default nil)
;; Server
(require 'server)
(unless (server-running-p)
  (server-start)
  )
;;
;; 文字コード設定
;;
;(set-language-environment 'Japanese)    ; 日本語環境
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)    ; UTF-8 が基本
(set-terminal-coding-system 'utf-8)    ; emacs -nw も文字化けしない
(set-keyboard-coding-system 'utf-8)   ;
(set-buffer-file-coding-system 'utf-8)   ;
(setq default-file-name-coding-system 'utf-8)

; 区切り文字に全角スペースや、・を含める
(setq paragraph-start '"^\\([ 　・○<\t\n\f]\\|(?[0-9a-zA-Z]+)\\)")

;; キーストロークの表示速度を上げる
(setq echo-keystrokes 0.1)

;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;; @ 前回編集していた場所を記憶し，ファイルを開いた時にそこへカーソルを移動
(require 'saveplace)
(setq-default save-place t)

;; ファイルのカーソル内の位置を記憶
(setq-default save-place t)

;; カーソルの位置が何文字目かをを表示する
(column-number-mode t)

;; カーソルの位置が何行目かを表示する
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

;; カーソルの点滅をやめる
(blink-cursor-mode 0)

;; カーソル行をハイライトする
(require 'hl-line)


;; 括弧
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)
(set-face-attribute 'show-paren-match-face nil
                    :background nil :foreground nil
                    :underline "#6c71c4" :weight 'extra-bold)
;; ウィンドウ内に収まらないときだけ、カッコ内も光らせる

;; スペース、タブなどを可視化する
(global-whitespace-mode 1)
(set-face-background 'trailing-whitespace "#b14770");赤紫

;; 全角スペースを強制表示する
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style '(face
                         trailing
                         tabs
                         spaces
                         empty
                         space-mark
                         tab-mark
                         ))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(setq whitespace-space-regexp "\\(\u3000+\\)")
;;; 各要素の face 設定
(set-face-attribute 'whitespace-space nil
                    :foreground "green"
                    :background 'unspecified)
(set-face-attribute 'whitespace-tab nil
                    :foreground "purple"
                    :background 'unspecified
                    :underline t)
(set-face-attribute 'whitespace-trailing nil
                    :foreground "purple"
                    :background 'unspecified
                    :underline t)

;;; ファイル保存時に行末のスペースを除去
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; tab
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
(defvar ctl-q-map (make-keymap)) ;; ctrl-q-map という変数
(define-key ctl-q-map (kbd "4") (lambda () (interactive) (set-aurora-tab-width 4 t t))) ; カレントバッファのタブを4 に
;; スクロールは１行ごとに
(setq scroll-conservatively 1)
;; beep音を消す
(setq ring-bell-function 'ignore)
;; emacsのwindow幅に合わせて、分割数を変える
;; 270以上であれば3分割
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;; company
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(defun company--insert-candidate2 (candidate)
  (when (> (length candidate) 0)
    (setq candidate (substring-no-properties candidate))
    (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
        (insert (company-strip-prefix candidate))
      (if (equal company-prefix candidate)
          (company-select-next)
          (delete-region (- (point) (length company-prefix)) (point))
        (insert candidate))
      )))

(defun company-complete-common2 ()
  (interactive)
  (when (company-manual-begin)
    (if (and (not (cdr company-candidates))
             (equal company-common (car company-candidates)))
        (company-complete-selection)
      (company--insert-candidate2 company-common))))

(define-key company-active-map [tab] 'company-complete-common2)
(define-key company-active-map [backtab] 'company-select-previous) ; おまけ
(setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))

;; flycheck
(require 'flycheck)

(global-flycheck-mode)

(define-key global-map (kbd "\C-cn") 'flycheck-next-error)
(define-key global-map (kbd "\C-cp") 'flycheck-previous-error)
(define-key global-map (kbd "\C-cd") 'flycheck-list-errors)

;;
;; AUCTeX
;;
(with-eval-after-load 'tex-jp
  (dolist (command '("pTeX" "pLaTeX" "pBibTeX" "jTeX" "jLaTeX" "jBibTeX" "Mendex"))
    (delq (assoc command TeX-command-list) TeX-command-list)))
(setq japanese-TeX-engine-default 'uptex)
(setq japanese-LaTeX-default-style "bxjsarticle")
;(setq japanese-LaTeX-default-style "jlreq")
(setq TeX-engine 'uptex)
(setq TeX-PDF-from-DVI "Dvipdfmx")
(setq TeX-view-program-selection '((output-pdf "Zathura")))
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)
(setq TeX-source-correlate-mode t)
(add-hook 'LaTeX-mode-hook 'japanese-latex-mode)
(with-eval-after-load 'tex-jp
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))
(add-hook 'LaTeX-mode-hook
          (function (lambda ()
          ;; Latexmk	C-c C-c latexmk RET	タイプセット (Latexmk)
                      (add-to-list 'TeX-command-list
                                   '("Latexmk"
                                     "latexmk %t"
                                     TeX-run-TeX nil (latex-mode) :help "Run Latexmk"))
                      ;; Latexmk-upLaTeX	C-c C-c latexmk-uplatex RET	タイプセット (Latexmk-upLaTeX)
                      (add-to-list 'TeX-command-list
                                   '("Latexmk-upLaTeX"
                                     "latexmk -e '$latex=q/uplatex %%O %(file-line-error) %(extraopts) %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %t"
                                     TeX-run-TeX nil (latex-mode) :help "Run Latexmk-upLaTeX"))
                      ;; Latexmk-LuaLaTeX	C-c C-c latexmk-lualatex RET	タイプセット (Latexmk-LuaLaTeX)
                      (add-to-list 'TeX-command-list
                                   '("Latexmk-LuaLaTeX"
                                     "latexmk -e '$lualatex=q/lualatex %%O %(file-line-error) %(extraopts) %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -norc -gg -pdflua %t"
                                     TeX-run-TeX nil (latex-mode) :help "Run Latexmk-LuaLaTeX"))
                      ;; Zathura	C-c C-c zathura RET	zathura で forward and inverse search
                      (add-to-list 'TeX-command-list
                                   '("Zathura"
                                     "zathura -x \"emacsclient --no-wait +%%{line} %%{input}\" --synctex-forward \"%n:0:%b\" %s.pdf"
                                     TeX-run-discard-or-function t t :help "Forward and inverse search with zathura"))
)))

;;
;; RefTeX with AUCTeX
;;
(with-eval-after-load 'tex-jp
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))
(setq reftex-plug-into-AUCTeX t)

(setq-default TeX-master nil)
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.

;; 禁則処理
;; 禁則処理によって行長が何文字延びてもいいかは，Emacs の kinsoku.el の kinsoku-limit で定義されています．
;; デフォルトでは 4 ですが，少し長くしておかないと，単語の途中で切れてしまうことがあるかもしれません．
(setq kinsoku-limit 10)

; タイプセット・プレビュー
;メニュー [Command]	入力	機能
;リストから選択	C-c C-c	コマンドの実行
;LaTeX	C-c C-c latex RET	指定された TeX エンジンでタイプセット
;Dvipdfmx	C-c C-c dvipdfmx RET	dvipdfmx の実行
;View	C-c C-c view RET	プレビュー または forward search
;BibTeX	C-c C-c bibtex RET	bibtex の実行
;Biber	C-c C-c biber RET	biber の実行
;Index	C-c C-c index RET	makeindex の実行
;Xindy	C-c C-c xindy RET	texindy の実行
;Check	C-c C-c check RET	lacheck の実行
;ChkTeX	C-c C-c chktex RET	chktex の実行
;Spell	C-c C-c spell RET	スペルチェックの実行
;Clean	C-c C-c clean RET	中間ファイル (.log .aux etc...) の削除
;Clean All	C-c C-c clean all RET	中間ファイル及び dvi ps pdf の削除
;Recenter Output Buffer	C-c C-l	タイプセット処理を表示
;Kill Job	C-c C-k	タイプセット処理を中断
;Next Error	C-c `	エラー行にジャンプ
;Quick View	C-c C-v	クイックビュー
;Compile and view	C-c C-a	タイプセットとプレビュー または forward search

; TeX エンジンの選択
;メニュー [Command]	入力	TeX エンジンの選択
;TeXing Options -> Use LuaTeX engine	M-x TeX-engine RET luatex RET	LuaTeX を使用
;TeXing Options -> Use upTeX engine	M-x TeX-engine RET uptex RET	upTeX を使用

;メニュー [Command]	入力	機能
;TeXing Options -> Generate PDF	C-c C-t C-p	TeX-PDF-mode を有効 ←→ 無効

;TeX-interactive-mode †
;TeX-interactive-mode を有効にするとエラーが発生した場合にユーザーからの応答を待つようになります．
;(add-hook 'LaTeX-mode-hook 'TeX-interactive-mode)
;メニュー [Command]	入力	機能
;TeXing Options -> Run Interactively	C-c C-t C-i	TeX-interactive-mode を有効 ←→ 無効

;TeX-source-correlate-mode †
;TeX-source-correlate-mode を有効にすると SyncTeX が使用可能になります．

;(setq TeX-source-correlate-method 'synctex)
;(setq TeX-source-correlate-start-server t)
;(setq TeX-source-correlate-mode t)
;メニュー [Command]	入力	機能
;TeXing Options -> Correlate I/O	C-c C-t C-s	TeX-source-correlate-mode を有効 ←→ 無効

;補完 †
;メニュー [LaTeX]	入力	補完
;Insert Environment (C-c C-e) -> リストから選択	C-c C-e	\begin{...} ... \end{...} の補完
;Section (C-c C-s) -> リストから選択	C-c C-s	\chapter, \section などの補完

;簡単な使い方 †
;まず C-c C-e すると，\documentclass{...} およびトップレベルの環境 \begin{document} ... \end{document} が入ります。
;次に C-c C-s すると，\section の類のコマンドが入ります。
;次に C-c C-e すると，\begin{...} ... \end{...} のような環境が入ります。 環境名は補完できます。
;次に C-c C-c するとコンパイルできます。
;次に C-c C-c すると依存関係を調べて出力ファイルが新しければ dvipdfmx を実行したりプレビューしたりします。
;dvipdfmx が実行された場合は次に C-c C-c するとプレビューします。
;再度コンパイルしたいのなら C-c C-c l で TAB or space を打てば LaTeX の文字が補完され RET を押すと LaTeX の処理が行われます。

;C-c C-a するとタイプセットからプレビューまでを一気に行います。

;いろいろな使い方 †
;ダブルクォート " を打てば，状況に応じて `` または '' に変換してくれます。

;TAB または SPACE で補完ができます。

;これらが効かないときは ESC TAB で補完します。

;候補一覧が出たら，さらに文字を補って補完するか， あるいはマウスの中央のボタンで候補をクリックします。

;フォント関係のコマンドです。
;メニュー [LaTeX]	入力	補完	補完 (数式モード)
;Insert Font -> Roman	C-c C-f C-r	\textrm{}	\mathrm{}
;Insert Font -> Italic	C-c C-f C-i	\textit{}	\mathit{}
;Insert Font -> Typewriter	C-c C-f C-t	\texttt{}	\mathtt{}
;Insert Font -> Bold	C-c C-f C-b	\textbf{}	\mathbf{}
;Insert Font -> Sans Serif	C-c C-f C-f	\textsf{}	\mathsf{}
;その他のコマンドです。
;TAB インデント
;C-j インデントして改行
;M-q 段落の整形
;M-x LaTeX-fill-buffer バッファ全体の整形
;C-c ; 領域のコメント
;C-c : 領域のコメントを外す
;C-c C-r 領域のコンパイル／プレビュー
;C-c ` エラー行に飛ぶ（日本語でエラーメッセージを表示）

(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
;; 辞書ファイルを環境に合わせて設定してください！
(setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
;(load-library "migemo")
(migemo-init)

;; @@ 基本の設定
(require 'skk)
(setq default-input-method "japanese-skk")
(setq skk-use-azik t)
(setq skk-tut-file "~/SKK.tut")
;(global-set-key (kbd "zenkaku-hankaku") 'toggle-input-method)
(global-set-key [zenkaku-hankaku] #'toggle-input-method)
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xt" 'skk-tutorial)


(unless window-system
  frame-background-mode 'dark)		; or 'light

;; ~/.skk にいっぱい設定を書いているのでバイトコンパイルしたい
;(setq skk-byte-compile-init-file t)
;; 注) 異なる種類の Emacsen を使っている場合は nil にします

;; SKK を Emacs の input method として使用する
(setq default-input-method
      "japanese-skk"			; (skk-mode 1)
;;    "japanese-skk-auto-fill"		; (skk-auto-fill-mode 1)
      )

;; SKK を起動していなくても、いつでも skk-isearch を使う
(setq skk-isearch-mode-enable 'always)

;; migemo を使うから skk-isearch にはおとなしくしていて欲しい
(setq skk-isearch-start-mode 'latin)

;; 文章系のバッファを開いた時には自動的に英数モード(「SKK」モード)に入る
(let ((function #'(lambda ()
		    (require 'skk)
		    (skk-latin-mode-on))))
  (dolist (hook '(find-file-hooks
		  ;; ...
		  mail-setup-hook
		  message-setup-hook))
    (add-hook hook function)))

;; Emacs 起動時に SKK を前もってロードする
(setq skk-preload t)
;; 注) skk.el をロードするだけなら (require 'skk) でもよい。上記設定の
;; 場合は、skk-search-prog-list に指定された辞書もこの時点で読み込んで
;; 準備する。Emacs の起動は遅くなるが，SKK を使い始めるときのレスポンス
;; が軽快になる。

; 背景色の設定
(defvar eww-disable-colorize t)
(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  (unless eww-disable-colorize
    (funcall orig start end fg)))
(advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
(advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
(defun eww-disable-color ()
  "eww で文字色を反映させない"
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload))
(defun eww-enable-color ()
  "eww で文字色を反映させる"
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload))
; search engine config
(setq eww-search-prefix "https://duckduckgo.com/html/?kl=jp-jp&k1=-1&kc=1&kf=-1&q=")
; eww 複数起動する
(defun eww-mode-hook--rename-buffer ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
(add-hook 'eww-mode-hook 'eww-mode-hook--rename-buffer)

; 現在の url を 外部ブラウザ で開く
(setq browse-url-generic-program (executable-find "google-chrome-stable"))
(setq shr-external-browser 'browse-url-generic)

; 現在の url を eww で開く
(defun browse-url-with-eww ()
  (interactive)
  (let ((url-region (bounds-of-thing-at-point 'url)))
    ;; url
    (if url-region
      (eww-browse-url (buffer-substring-no-properties (car url-region)
						      (cdr url-region))))
    ;; org-link
    (setq browse-url-browser-function 'eww-browse-url)
    (org-open-at-point)))
(global-set-key (kbd "C-c p") 'browse-url-with-eww)

; 画像表示
(defun eww-disable-images ()
  "eww で画像表示させない"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image-alt)
  (eww-reload))
(defun eww-enable-images ()
  "eww で画像表示させる"
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image)
  (eww-reload))
(defun shr-put-image-alt (spec alt &optional flags)
  (insert alt))
;; はじめから非表示
(defun eww-mode-hook--disable-image ()
  (setq-local shr-put-image-function 'shr-put-image-alt))
(add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)
; sh-mode
(setq sh-basic-offset 2)
(setq sh-indentation 2)
(setq sh-shell-file "/bin/sh")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (migemo flycheck ddskk company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

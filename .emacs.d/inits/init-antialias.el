;(cond ((equal window-system t)    ; GUI時
(load-theme 'misterioso t)
;Font
(create-fontset-from-ascii-font "Ricty-18:weight=normal:slant=normal" nil "Ricty18")
(set-fontset-font "fontset-Ricty18" 'unicode "Ricty-18:weight=normal:slant=normal" nil 'append)
(add-to-list 'default-frame-alist '(font . "fontset-Ricty18"))

;; mode-lineを目立たせる(Fig3)
 (setq solarized-high-contrast-mode-line t)
;; ウィンドウを透明にする
;; アクティブウィンドウ／非アクティブウィンドウ（alphaの値で透明度を指定）
(add-to-list 'default-frame-alist '(alpha . (90 85)))
;; underline
(setq x-underline-at-descent-line t)
;; Key bindings
;; メニューバーを消す
(menu-bar-mode -1)
;; ツールバーを消す
(tool-bar-mode -1)
;; スクロールバーを消す
(scroll-bar-mode -1)

;; クリップボード共有
(setq x-select-enable-clipboard t)
  ;; 現在行に色をつける
  (global-hl-line-mode t)		  ; 現在行に色をつける
;    )
;)

;; font-lock-mode
(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

;; set default frame parameters
;; (add-to-list 'default-frame-alist '(alpha 90 80))
;; (add-to-list 'default-frame-alist '(background-color . "gray8"))
;; (add-to-list 'default-frame-alist '(foreground-color . "white"))
;; (add-to-list 'default-frame-alist '(font . "Monospace-10"))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)
(display-time-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; key bindings
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key key-translation-map [?\C-\]] [?\C-h])
;(global-set-key "\C-c?" 'help-command)
(global-set-key "\C-x~" 'dirs)
(global-set-key "\C-c%" 'query-replace-regexp)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-ct" 'tags-search)
(global-set-key "\C-cv" 'view-mode)
(global-set-key "\M- " 'set-mark-command)
;(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x5k" 'delete-frame)
(global-set-key "\C-cK" 'kill-buffer-and-window)
(global-set-key [M-down] 'next-error)
(global-set-key [M-up] '(lambda () (interactive) (next-error -1)))
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-x." 'repeat)
(global-set-key "\C-x\C-n" 'switch-to-next-buffer)
(global-set-key "\C-x\C-p" 'switch-to-prev-buffer)
(global-set-key "\C-cB" 'browse-url-at-point)

;; display settings
(line-number-mode 1)
(column-number-mode t)
(when (not (window-system))
  (display-time))
(setq visible-bell t)
(global-hl-line-mode 1)
;; add line number for open files
;(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(setq use-dialog-box nil)

;; mode-line format
(setq-default mode-line-format '("%e" mode-line-front-space mode-line-modified mode-line-remote " " mode-line-buffer-identification " " mode-line-position (vc-mode vc-mode) mode-line-modes mode-line-misc-info mode-line-end-spaces))

;; Move to another window with S-<arrow>
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; (global-set-key (kbd "C-c <left>")  'windmove-left)
;; (global-set-key (kbd "C-c <right>") 'windmove-right)
;; (global-set-key (kbd "C-c <up>")    'windmove-up)
;; (global-set-key (kbd "C-c <down>")  'windmove-down)

;; make mouse clicks work in xterm
;(when (not (window-system))
;  (xterm-mouse-mode 1))

;; wheel mouse support
(when window-system
  ;; enable wheelmouse support by default
  (mwheel-install)
  ;; use extended compound-text coding for X clipboard
  (set-selection-coding-system 'compound-text-with-extensions))

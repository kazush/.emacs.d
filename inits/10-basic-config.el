;; dired
(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "k" 'dired-kill-subdir)))
(require 'wdired)
(setq wdired-allow-to-change-permissions t)
(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)

;; basic editor cutomistomization
(setq auto-window-vscroll nil)
(setq-default bidi-display-reordering nil)
(setq history-delete-duplicates t)
(setq set-mark-command-repeat-pop t)
(global-auto-revert-mode)
(transient-mark-mode t)
(show-paren-mode 1)
(setq mouse-yank-at-point t)
(setq backup-inhibited t)
(setq next-line-add-newlines nil)
(setq kill-whole-line t)
(setq require-final-newline nil)
(setq enable-kinsoku t)
(setq message-log-max 10000)
(setq history-length 1000)
(setq history-delete-duplicates t)
;; disable novice restrictions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
;; fill settings
;(add-hook 'text-mode-hook '(lambda () (auto-fill-mode 1)))
(setq-default fill-column 80)
(setq truncate-partial-width-windows nil)
;; resolve symlinks
(setq find-file-visit-truename t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "[^*]+")

;(require 'misc)
;(global-set-key (kbd "M-f") 'forward-to-word)
;(global-set-key (kbd "M-b") 'backward-word)

(winner-mode 1)
(setq buffer-quit-function 'winner-undo)

;; GC settings
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; display function name in mode line
;(which-function-mode 1)

;; language settings
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; enable completion in minibuffer by typing space
(if (boundp 'minibuffer-local-filename-completion-map)
    (define-key minibuffer-local-filename-completion-map
      " " 'minibuffer-complete-word))
(if (boundp 'minibuffer-local-must-match-filename-map)
    (define-key minibuffer-local-must-match-filename-map
      " " 'minibuffer-complete-word))

;; do chmod +x if script file
;; (idea by http://www-nagao.kuee.kyoto-u.ac.jp/member/tsuchiya/elisp/#script)
(add-hook 'after-save-hook
          '(lambda ()
             (save-restriction
               (widen)
               (if (string= "#!" (buffer-substring 1 (min 3 (point-max))))
                   (let ((name (buffer-file-name)))
                     (or
                      (char-equal ?. (string-to-char
                                      (file-name-nondirectory name)))
                      (let ((mode (file-modes name)))
                        (set-file-modes name
                                        (logior mode (logand (/ mode 4) 73)))
                        (message (concat "Wrote " name " (+x)"))))
                     )))))

;; jka-compr
(require 'jka-compr)
(auto-compression-mode 1)

;; tramp to access remote files transparently
(require 'tramp)
(setq tramp-default-method "ssh")

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; Easy PG
(setq epa-pinentry-mode 'loopback)

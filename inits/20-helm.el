(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (require 'helm-config)
  (helm-mode 1)
  (global-set-key "\C-ch" 'helm-command-prefix)
  :bind (("C-c h o" . helm-occur)
         ("C-c h r" . helm-recentf)
         ("C-c h %" . helm-regexp)
         ("C-c g" . helm-do-grep-ag)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x))
  :config
  (setq helm-completion-mode-string "")
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

  (defun my/helm-buffers-list (sources)
    "Dispatch helm with specified sources"
    (helm :sources sources
          :buffer "*helm buffers*"
          :keymap helm-buffer-map
          :truncate-lines helm-buffers-truncate-lines))
  )

(use-package helm-descbinds
  :ensure t
  :config (helm-descbinds-mode))

(use-package helm-describe-modes
  :ensure t
  :config (global-set-key [remap describe-mode] #'helm-describe-modes))

(use-package helm-swoop
  :ensure t
  :bind (("C-c O" . helm-swoop)))

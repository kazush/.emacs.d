
(use-package ycmd
  :ensure t
  :config
  (set-variable 'ycmd-server-command
                (list "python"
                      (substitute-in-file-name "$HOME/ws/ycmd/ycmd/__main__.py")))
  (setq request-message-level -1)
  (setq ycmd-parse-conditions '(save new-line mode-enabled idle-change))
  (setq url-show-status nil)
  (add-hook 'emacs-lisp-mode-hook
            '(lambda () (ycmd-mode 0)))
  (add-hook 'after-init-hook #'global-ycmd-mode))

(use-package company
  :ensure t
  :config
  (setq company-lighter "")
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0.2)
  (setq company-echo-delay 0)
  (add-hook 'prog-mode-hook
            '(lambda () (company-mode)))
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  )

(use-package company-ycmd
  :ensure t
  :config (company-ycmd-setup))

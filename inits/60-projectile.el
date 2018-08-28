(use-package projectile
  :ensure t
  :config
  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-hook 'comint-mode-hook
            (lambda () (projectile-mode 0)))
  (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

(use-package projectile-speedbar
  :ensure t
  :bind ("C-c I p" . projectile-speedbar-open-current-buffer-in-tree))

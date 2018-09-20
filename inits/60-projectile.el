(use-package projectile
  :ensure t
  :hook ((prog-mode . projectile-mode)
         (comint-mode . (lambda () (projectile-mode 0))))
  :config
  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

(use-package projectile-speedbar
  :ensure t
  :bind ("C-c I p" . projectile-speedbar-open-current-buffer-in-tree))

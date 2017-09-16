(use-package projectile
  :ensure t
  :config
  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  (projectile-mode))
(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))
(use-package projectile-speedbar
  :ensure t
  :bind ("C-c i p" . projectile-speedbar-open-current-buffer-in-tree))

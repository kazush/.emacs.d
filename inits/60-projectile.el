(use-package projectile
  :diminish
  :hook ((prog-mode . projectile-mode)
         (comint-mode . (lambda () (projectile-mode 0))))
  :config
  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system
        (if my/enable-helm 'helm 'ivy)))

(use-package helm-projectile
  :if my/enable-helm
  :config (helm-projectile-on))

(use-package projectile-speedbar
  :bind ("C-c I p" . projectile-speedbar-open-current-buffer-in-tree))

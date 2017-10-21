(use-package magit
  :ensure t
  :commands (magit-status)
  :bind (("C-c m" . magit-status)))

(use-package helm-ls-git
  :ensure t
  :commands (helm-ls-git-ls helm-browse-project)
  :init
  (eval-after-load 'helm
    '(define-key helm-map (kbd "C-x C-d") 'helm-ls-git-ls)))

(use-package helm-git-grep
  :ensure t
  :commands (helm-git-grep helm-git-grep-from-helm)
  :bind (("C-c h g" . helm-git-grep))
  :init
  (define-key isearch-mode-map (kbd "C-c h g")
    'helm-git-grep-from-isearch)
  (eval-after-load 'helm
    '(define-key helm-map (kbd "C-c h g") 'helm-git-grep-from-helm)))

(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:lighter "")
  (global-git-gutter-mode 1)
;  (git-gutter:linum-setup)
  )

;; (install-package 'magithub)
;; (use-package magithub
;;   :after magit
;;   :config (magithub-feature-autoinject t))

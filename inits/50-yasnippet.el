(use-package yasnippet
  :ensure t
  :config
  (setcar (cdr (assoc 'yas-minor-mode minor-mode-alist)) "")
  (yas-global-mode 1))

(use-package angular-snippets :ensure t)
(use-package go-snippets :ensure t)
(use-package helm-c-yasnippet :ensure t)
(use-package java-snippets :ensure t)
(use-package auto-yasnippet :ensure t)

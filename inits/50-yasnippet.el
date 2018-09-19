(use-package yasnippet
  :ensure t
  :config
  (setcar (cdr (assoc 'yas-minor-mode minor-mode-alist)) "")
  (yas-global-mode 1))

(use-package auto-yasnippet
  :ensure t
  :bind (("C-c y c" . aya-create)
         ("C-c y C" . aya-create-one-line)
         ("C-c y y" . aya-expand)
         ("C-c y o" . aya-open-line)))

(use-package yasnippet-snippets :ensure t :commands prog-mode)
(use-package angular-snippets :ensure t :commands ng2-mode)
(use-package go-snippets :ensure t :commands go-mode)
(use-package helm-c-yasnippet :ensure t :commands c-mode)
(use-package java-snippets :ensure t :commands java-mode)

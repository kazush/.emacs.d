(use-package yasnippet
  :config
  (setcar (cdr (assoc 'yas-minor-mode minor-mode-alist)) "")
  (yas-global-mode 1))

(use-package auto-yasnippet
  :bind (("C-c y c" . aya-create)
         ("C-c y C" . aya-create-one-line)
         ("C-c y y" . aya-expand)
         ("C-c y o" . aya-open-line)))

(use-package yasnippet-snippets :commands prog-mode)
(use-package angular-snippets :commands ng2-mode)
(use-package go-snippets :commands go-mode)
(use-package java-snippets :commands java-mode)
(use-package helm-c-yasnippet
  :if my/enable-helm
  :commands c-mode)

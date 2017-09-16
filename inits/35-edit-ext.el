;; expand-region
(use-package expand-region
  :ensure t
  :bind (("M-SPC" . er/expand-region)))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-x r t" . mc/edit-lines)
         ("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)
         ("C-c /" . mc/mark-all-like-this)))

;; wgrep
(use-package wgrep :ensure t)
(use-package wgrep-helm :ensure t)
(use-package wgrep-ag :ensure t)

;; undo tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff 1)
  (setq undo-tree-visualizer-timestamps 1)
  (define-key undo-tree-map (kbd "C-x u") 'undo-tree-undo)
  (define-key undo-tree-map (kbd "C-_") 'undo-tree-visualize)
  (setq undo-tree-mode-lighter ""))

;; iedit
(use-package iedit
  :ensure t
  :bind (("C-c ;" . iedit-mode))
  :config
  (define-key iedit-mode-keymap (kbd "C-o") 'iedit-toggle-unmatched-lines-visible))

;; ws-butler
(use-package ws-butler
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (setcar (cdr (assoc 'ws-butler-mode minor-mode-alist)) ""))

;; expand-region
(use-package expand-region
  :ensure t
  :bind (("M-SPC" . er/expand-region)))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :config
  (defhydra multiple-cursors-hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))
  )

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

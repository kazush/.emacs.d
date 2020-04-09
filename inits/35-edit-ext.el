;; expand-region
(use-package expand-region
  :bind (("M-SPC" . er/expand-region)))

;; iedit
(use-package iedit
  :config
  (define-key iedit-mode-keymap (kbd "C-o") 'iedit-toggle-unmatched-lines-visible))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-c ;" . hydra-multiple-cursors/body))
  :init
  (defhydra hydra-multiple-cursors (:body-pre (mc/mark-all-like-this-dwim nil)
                                              :hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             [_i_]   iEdit   [_q_] Quit
"
    ("i" iedit-mode :exit t)
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
(use-package wgrep
  :if my/enable-helm)
(use-package wgrep-ag
  :if my/enable-helm)
(use-package wgrep-helm
  :if my/enable-helm)

;; undo tree
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff 1)
  (setq undo-tree-visualizer-timestamps 1)
  (define-key undo-tree-map (kbd "C-x u") 'undo-tree-undo)
  (define-key undo-tree-map (kbd "C-_") 'undo-tree-visualize)
  (setq undo-tree-mode-lighter ""))

;; ws-butler
(use-package ws-butler
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (setcar (cdr (assoc 'ws-butler-mode minor-mode-alist)) ""))

;; volatile-highlights
(use-package volatile-highlights
  :diminish
  :config
  (volatile-highlights-mode t))

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
  (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                        :hint nil)
    "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
    ("j" git-gutter:next-hunk)
    ("k" git-gutter:previous-hunk)
    ("h" (progn (goto-char (point-min))
                (git-gutter:next-hunk 1)))
    ("l" (progn (goto-char (point-min))
                (git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("p" git-gutter:popup-hunk)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (progn (git-gutter-mode -1)
                ;; git-gutter-fringe doesn't seem to
                ;; clear the markup right away
                (sit-for 0.1)
                (git-gutter:clear))
     :color blue))
  )

;; (install-package 'magithub)
;; (use-package magithub
;;   :after magit
;;   :config (magithub-feature-autoinject t))

(use-package git-timemachine
  :ensure t)

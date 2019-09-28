(use-package elfeed
  :bind (("C-c F" . elfeed)))

(use-package elfeed-goodies
  :config
  (elfeed-goodies/setup)
  (setq elfeed-goodies/entry-pane-position 'bottom)
  (setq elfeed-goodies/entry-pane-size 0.8))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

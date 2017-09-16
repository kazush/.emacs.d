(use-package restclient :ensure t)
(use-package restclient-helm :ensure t)
(use-package company-restclient
  :ensure t
  :config
  (add-to-list 'company-backends 'company-restclient)
  (add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode)))
;; restclient support for org babel
(use-package ob-restclient
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

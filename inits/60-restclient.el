(use-package restclient :ensure t)
(use-package restclient-helm
  :if my/enable-helm)
(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient)
  (add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode)))

;; restclient support for org babel
(use-package ob-restclient
  :config
  (require 'ob-restclient))

(use-package ivy
  :unless my/enable-helm
  :diminish
  :bind (("C-c g" . counsel-ag))
  :config
  (define-prefix-command 'my-ivy-map)
  (global-set-key (kbd "C-c h") 'my-ivy-map)

  (require 'ivy-hydra)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (ivy-mode))

(use-package ivy-hydra
  :unless my/enable-helm)

(use-package ivy-rich
  :unless my/enable-helm
  :diminish
  :after ivy
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  ;; (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode 1))

(use-package counsel
  :unless my/enable-helm
  :diminish
  :after ivy
  :bind (("C-c h r" . counsel-recentf)
         ("C-c h g" . counsel-ag))
  :config
  ;; (define-key my-ivy-map (kbd "r") #'counsel-recentf)
  ;; (define-key my-ivy-map (kbd "g") #'counsel-grep)

  (setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))
  (counsel-mode))

(use-package swiper
  :unless my/enable-helm
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package smex
  :unless my/enable-helm
  :config
  (smex-initialize))

(use-package counsel-projectile
  :unless my/enable-helm
  :config
  (counsel-projectile-mode))

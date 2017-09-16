;; compilation
(setq compilation-scroll-output 'first-error)

;; linum-mode
;(setq linum-format "%4d\u2502")
;(add-hook 'prog-mode-hook
;          '(lambda () (linum-mode 1)))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))
(use-package flycheck-ycmd
  :ensure t
  :config
  (add-hook 'after-init-mode #'flycheck-ycmd-setup))

;; Do not use TAB for indentation in prog mode
(add-hook 'prog-mode-hook
          '(lambda ()
             (setq-default indent-tabs-mode nil)))

;; c/c++-mode
(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))
(use-package modern-cpp-font-lock
  :ensure t
  :config
  (modern-c++-font-lock-global-mode t))
(use-package company-c-headers
  :ensure t
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-c-headers)))

;; javascript-mode
(add-hook 'js-mode-hook '(lambda ()
                           (setq js-indent-level 2)
                           (setq tab-width 2)))

;; angular-mode
(use-package angular-mode :ensure t)

;; go-mode
(use-package go-mode
  :ensure t
  :mode (("\\.go$" . go-mode))
  :init
  (add-hook 'go-mode-hook
            '(lambda ()
               (ycmd-mode)
               (company-mode)
               (flycheck-mode)
               (go-guru-hl-identifier-mode)
               (go-eldoc-setup)
               (add-hook 'before-save-hook 'gofmt-before-save)
               (local-set-key (kbd "M-.") 'godef-jump)
               (local-set-key (kbd "M-*") 'pop-tag-mark)
               (set (make-local-variable 'compile-command)
                    "go build -v && go test -v && go vet")
               (setq gofmt-command "goimports")
               (setq fill-column 120)
               (setq tab-width 4)
               (setq indent-tabs-mode 1))))

(use-package go-guru :ensure t)
(use-package golint :ensure t)
(use-package helm-go-package
  :ensure t
  :init
  (eval-after-load 'go-mode
    '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map)))

;; anaconda-mode for python
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
(use-package company-anaconda
  :ensure t
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))


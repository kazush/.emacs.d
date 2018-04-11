;; C/C++

;; Split window and display compilation buffer below the original window.
;; (add-to-list 'display-buffer-alist
;;              `(,(rx bos "*compilation" (* not-newline) "*" eos)
;;                (display-buffer--maybe-pop-up-frame-or-window)))

;; elisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda () (ycmd-mode 0)))

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
(use-package clang-format
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (local-set-key (kbd "C-c i b") 'clang-format-buffer)
               (local-set-key (kbd "C-c i r") 'clang-format-region)))
  (setq clang-format-style-option "google"))

;; Javascript

;; javascript-mode
(add-hook 'js-mode-hook '(lambda ()
                           (setq-local js-indent-level 2)
                           (setq-local tab-width 2)))

;; angular-mode
(use-package angular-mode :ensure t)

;; Go

;; go-mode
(use-package go-mode
  :ensure t
  :mode (("\\.go$" . go-mode))
  :config
  (add-hook 'go-mode-hook
            '(lambda ()
               (setq-local compile-command
                           "go build -v && go test -v && go vet")
               (setq-local gofmt-command "goimports")
               (setq-local fill-column 100)
               (setq-local c-basic-offset 4)
               (setq-local tab-width 4)
               (setq-local indent-tabs-mode 1)
               (ycmd-mode)
               (company-mode)
               (flycheck-mode)
               (go-guru-hl-identifier-mode)
               (add-hook 'before-save-hook 'gofmt-before-save)
               (local-set-key (kbd "M-.") 'godef-jump)
               (local-set-key (kbd "M-*") 'pop-tag-mark))))

;; (use-package go-eldoc
;;   :ensure t
;;   :config
;;   (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-guru :ensure t)
(use-package golint :ensure t)
(use-package helm-go-package
  :ensure t
  :init
  (eval-after-load 'go-mode
    '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map)))

;; Python

;; yapf
(use-package yapfify
  :ensure t
  :config
  ;; (add-hook 'python-mode-hook 'yapf-mode)
  (add-hook 'python-mode-hook
            '(lambda () (local-set-key (kbd "C-c i b") 'yapfify-buffer))))

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

;; https://github.com/jorgenschaefer/elpy/issues/887
(defun python-shell-completion-native-try ()
  "Return non-nil if can trigger native completion."
  (with-eval-after-load 'python
    '(let ((python-shell-completion-native-enable t)
           (python-shell-completion-native-output-timeout
            python-shell-completion-native-try-output-timeout))
       (python-shell-completion-native-get-completions
        (get-buffer-process (current-buffer))
        nil "_"))))

;; Java

(add-hook 'java-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c i b") 'google-java-format-buffer)
             (local-set-key (kbd "C-c i r") 'google-java-format-region)))

;;; 30-prog-modes.el ends here

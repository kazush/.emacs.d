;; C/C++

;; Split window and display compilation buffer below the original window.
;; (add-to-list 'display-buffer-alist
;;              `(,(rx bos "*compilation" (* not-newline) "*" eos)
;;                (display-buffer--maybe-pop-up-frame-or-window)))

(defvar my/compilation-last-buffer nil)
(defun my/get-compilation-buffer (arg)
  (interactive "p")
  (let* ((b (get-buffer "*compilation*"))
         (w (get-buffer-window b)))
    (if (and w (= arg 4))
        (delete-window w)
      (if (and b (eq b (current-buffer)))
          (select-window (display-buffer my/compilation-last-buffer))
        (cond (b
               (setq my/compilation-last-buffer (current-buffer))
               (select-window (display-buffer b))))))))
(global-set-key (kbd "\C-c C") #'my/get-compilation-buffer)

;; elisp
(add-hook 'emacs-lisp-mode-hook
          '(lambda () (ycmd-mode 0)))

;; c/c++-mode
(use-package google-c-style
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))
(use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode t))
(use-package company-c-headers
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-c-headers)))
(use-package clang-format
  :config
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (local-set-key (kbd "C-c i b") 'clang-format-buffer)
               (local-set-key (kbd "C-c i r") 'clang-format-region)))
  (setq clang-format-style-option "google"))

;; MQL4
(add-to-list 'auto-mode-alist '("\\.mq[45h]$" . c++-mode))

;; Web-mode (HTML+CS/JS)
(use-package web-mode
  :mode ("\\.p?html?\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'")
  :hook ((web-mode . (lambda ()
                       (setq-local indent-tabs-mode nil))))
  :after (smartparens)
  :config
  (define-key web-mode-map (kbd "C-c /") 'web-mode-element-close)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-opening t)
  (setq web-mode-enable-auto-quoting t)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight nil)
  (setq web-mode-enable-current-column-highlight nil)
  (setq web-mode-enable-comment-interpolation t)

  (set-face-foreground 'web-mode-current-element-highlight-face "orange")
  (set-face-foreground 'web-mode-html-entity-face "yellow")
  (set-face-foreground 'web-mode-html-tag-face "orangered")
  (set-face-foreground 'web-mode-html-tag-bracket-face
                       (color-darken-name (face-foreground 'default) 20))
  (set-face-foreground 'web-mode-html-attr-name-face "orange")
  (set-face-foreground 'web-mode-html-attr-equal-face "cyan")
  (set-face-foreground 'web-mode-html-attr-value-face
                       (face-foreground 'font-lock-constant-face))
  ;; (set-face-foreground 'web-mode-annotation-tag-face "lightblue")
  ;; (set-face-foreground 'web-mode-annotation-type-face "pink")
  ;; (set-face-foreground 'web-mode-annotation-value-face "navy")
  ;; (set-face-foreground 'web-mode-constant-face "limegreen")
  ;; (set-face-foreground 'web-mode-filter-face "darkblue")
  ;; (set-face-foreground 'web-mode-keyword-face "pink")
  ;; (set-face-foreground 'web-mode-symbol-face
  ;;                      (face-foreground 'font-lock-constant-face))
  ;; (set-face-foreground 'web-mode-type-face "navy")
  ;; (set-face-foreground 'web-mode-variable-name-face "lightblue")

  (defun sp-web-mode-is-code-context (id action context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))
  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))
  )

;; Javascript

;; style
(setq-default js-indent-level 2)
(setq-default tab-width 2)

;; rjsx-mode
(use-package rjsx-mode
  :mode ("\\.js\\'" "\\.jsx\\'"))

;; Typescript
(use-package tide
  :after (flycheck)
  :mode (("\\.tsx$" . web-mode))
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (web-mode . (lambda ()
                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
                         (tide-setup)
                         (tide-hl-identifier-mode))))
         (before-save . tide-format-before-save))
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode))

;; ng2-mode for Angular 2
(use-package ng2-mode :ensure t)

;; Go

;; go-mode
(use-package go-mode
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
  :if my/enable-helm
  :init
  (eval-after-load 'go-mode
    '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map)))

;; Python

;; yapf
(use-package yapfify
  :config
  ;; (add-hook 'python-mode-hook 'yapf-mode)
  (add-hook 'python-mode-hook
            '(lambda () (local-set-key (kbd "C-c i b") 'yapfify-buffer))))

;; anaconda-mode for python
(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
(use-package company-anaconda
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

;; Protobuf
(use-package protobuf-mode
  :after (smartparens)
  :hook (protobuf-mode . smartparens-mode))

;; sh-mode
(add-hook 'sh-mode-hook
          '(lambda ()
             (setq-local sh-basic-offset 2)
             (setq-local tab-width 2)
             (setq-local indent-tabs-mode 0)))

;;; 30-prog-modes.el ends here

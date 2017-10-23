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
(use-package flycheck-popup-tip
  :ensure t
  :config
  (flycheck-popup-tip-mode))

(use-package flycheck-ycmd
  :ensure t
  :config
  (add-hook 'after-init-mode #'flycheck-ycmd-setup))

;; Do not use TAB for indentation in prog mode
(add-hook 'prog-mode-hook
          '(lambda ()
             (setq-default indent-tabs-mode nil)))

;; smartparens
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :config
  (defun my-sp-pair-function (id action context)
    (if (eq action 'insert)
        ;; t to pair, nil to not pair
        (or (looking-at "[[:space:][:punct:]]")
            (sp-point-before-eol-p id action context))
      t))
  (sp-pair "(" ")" :when '(my-sp-pair-function) :wrap "C-c (")
  (sp-pair "{" "}" :when '(my-sp-pair-function) :wrap "C-c {")
  (sp-pair "[" "]" :when '(my-sp-pair-function) :wrap "C-c [")
  (sp-pair "\"" "\"" :when '(my-sp-pair-function) :wrap "C-c \"")
  (sp-pair "'" "'" :when '(my-sp-pair-function) :wrap "C-c '")
  (sp-pair "`" "`" :when '(my-sp-pair-function) :wrap "C-c `")
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)
  (setcar (cdr (assoc 'smartparens-mode minor-mode-alist)) "")
  (global-set-key "\C-c]" 'sp-unwrap-sexp)
  (sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

;; embrace
(use-package embrace
  :ensure t
  :bind (("C-c :" . embrace-commander)))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; show whitespaces
(use-package whitespace
  :ensure t
  :init
  (add-hook 'after-change-major-mode-hook
            '(lambda ()
               (when (member major-mode
                             '(emacs-lisp-mode
                               c-mode
                               c++-mode
                               go-mode
                               javascript-mode
                               js-mode
                               python-mode
                               html-mode
                               sgml-mode
                               xml-mode
                               ))
                 (whitespace-mode 1))))
  (add-hook 'go-mode-hook
            '(lambda ()
               (setq-local whitespace-style '(face trailing))))
  (add-hook 'font-lock-mode-hook
            '(lambda ()
               (set-face-background 'whitespace-tab "gray5")
               (set-face-background 'whitespace-trailing "gray25")))
  :config
  (setq whitespace-style '(face trailing tabs))
  (setcar (cdr (assoc 'whitespace-mode minor-mode-alist)) ""))

;; dumb-jump
(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'dumb-jump-mode))

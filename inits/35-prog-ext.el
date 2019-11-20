;; compilation
(setq compilation-scroll-output 'first-error)

;; linum-mode
;; (setq linum-format "%4d\u2502")
;; (add-hook 'prog-mode-hook
;;           '(lambda () (linum-mode 1)))

(use-package eldoc-box
  :diminish
  :hook ((prog-mode . eldoc-box-hover-mode))
  :config
  (setq eldoc-box-clear-with-C-g t))

(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq flycheck-indication-mode nil))

(use-package flycheck-popup-tip
  :ensure t
  :config
  (flycheck-popup-tip-mode))

;; Do not use TAB for indentation in prog mode
(add-hook 'prog-mode-hook
          '(lambda ()
             (setq-default indent-tabs-mode nil)))

;; smartparens
(use-package smartparens
  :bind (("C-M-f" . sp-forward-sexp)
         ("C-c >" . sp-slurp-hybrid-sexp)
         ("C-c }" . sp-rewrap-sexp))
  :init
  (require 'smartparens-config)
  :config
  (defun my-sp-pair-function (id action context)
    (if (eq action 'insert)
        ;; t to pair, nil to not pair
        (or (looking-at "[[:space:][:punct:]]")
            (sp-point-before-eol-p id action context))
      t))
  (defun my-sp-pair-less-than-function (id action context)
    (if (eq action 'insert)
        (and (eq major-mode 'web-mode)
             (my-sp-pair-function id action context))
      t))
  (sp-pair "(" ")" :when '(my-sp-pair-function) :wrap "C-c (")
  (sp-pair "{" "}" :when '(my-sp-pair-function) :wrap "C-c {")
  (sp-pair "[" "]" :when '(my-sp-pair-function) :wrap "C-c [")
  (sp-pair "<" ">" :when '(my-sp-pair-less-than-function) :wrap "C-c <")
  (sp-pair "\"" "\"" :when '(my-sp-pair-function) :wrap "C-c \"")
  (sp-pair "'" "'" :when '(my-sp-pair-function) :wrap "C-c '")
  (sp-pair "`" "`" :when '(my-sp-pair-function) :wrap "C-c `")
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)
  (setcar (cdr (assoc 'smartparens-mode minor-mode-alist)) "")
  (global-set-key "\C-c]" 'sp-unwrap-sexp)
  (sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'go-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (defun my-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

;; embrace
(use-package embrace
  :bind (("C-c :" . embrace-commander)))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (set-face-background 'rainbow-delimiters-unmatched-face "red"))

;; show whitespaces
(use-package whitespace
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
  :config
  (add-hook 'prog-mode-hook 'dumb-jump-mode))

(use-package aggressive-indent
  :config
  (add-hook 'go-mode-hook 'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package symbol-overlay
  :hook ((prog-mode) . symbol-overlay-mode)
  :config
  (setq symbol-overlay-idle-time 1.0))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :diminish highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'character))

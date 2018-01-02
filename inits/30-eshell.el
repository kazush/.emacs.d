;;; 30-eshell.el --- Configuration for eshell
;;; Commentary:
;;; Code:

(setq eshell-buffer-shorthand t
      eshell-scroll-to-bottom-on-input 'all
      eshell-error-if-no-glob t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil)

(add-hook 'eshell-mode-hook
          '(lambda ()
             ;; aliases
             (eshell/alias "ls" "ls -A $*")
             (eshell/alias "l" "ls -lA $*")
             (eshell/alias "ff" "find-file $1")
             (eshell/alias "e" "find-file $1")
             (eshell/alias "ms" "magit-status")
             (eshell/alias "gc" "git checkout $*")
             (eshell/alias "gb" "git branch $*")
             (eshell/alias "gs" "git status $*")
             (eshell/alias "gd" "git diff $*")
             ;; visual commands
             (add-to-list 'eshell-visual-commands "ssh")
             (add-to-list 'eshell-visual-commands "tail")
             (add-to-list 'eshell-visual-commands "top")
             (with-eval-after-load 'company
               (company-mode 1)
               (setq-local company-tooltip-limit 5)
               (setq-local company-idle-delay 1.0)
               (setq-local company-backends '(company-capf)))))

(defun myeshell ()
  (interactive)
  (eshell "new"))

(global-set-key (kbd "C-c e") #'myeshell)

(use-package eshell-z
  :ensure t)

(use-package eshell-git-prompt
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'powerline))

(provide '30-eshell)
;;; 30-eshell.el ends here

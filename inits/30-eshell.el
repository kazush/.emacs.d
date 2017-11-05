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
             (eshell/alias "ls" "ls -lA")
             (eshell/alias "l" "ls -lA")
             (eshell/alias "ff" "find-file $1")
             (eshell/alias "e" "find-file $1")
             ;; visual commands
             (add-to-list 'eshell-visual-commands "ssh")
             (add-to-list 'eshell-visual-commands "tail")
             (add-to-list 'eshell-visual-commands "top")
             (with-eval-after-load 'company
               (company-mode 1)
               (setq-local company-backends '(company-capf company-files)))))

(defun myeshell ()
  (interactive)
  (eshell "new"))

(global-set-key (kbd "C-c e") #'myeshell)

(use-package eshell-z
  :ensure t)

(provide '30-eshell)
;;; 30-eshell.el ends here

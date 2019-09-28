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
             (eshell/alias "ffsu" "find-file /sudo::$PWD/$1")
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

(defun eshell-cwd ()
  (interactive)
  (let ((cwd default-directory))
    (eshell)
    ;; (switch-to-buffer "*eshell*")
    (cd cwd)))

(defun myeshell ()
  (interactive)
  (eshell "new"))

(defun last-eshell-buffer (l)
  "Return most recently used eshell buffer."
  (when l
    (if (eq 'eshell-mode (with-current-buffer (car l) major-mode))
        (car l) (last-eshell-buffer (cdr l)))))

(defun get-eshell (arg)
  "Switch to the eshell buffer last used, or create a new one if
    none exists, or if the current buffer is already a eshell."
  (interactive "p")
  (let ((b (last-eshell-buffer (buffer-list))))
    (if (or (not b) (= arg 4))
        (eshell-cwd)
      (if (eq 'eshell-mode major-mode)
          (delete-window)
        (select-window (display-buffer b))))))

(global-set-key (kbd "C-c e") #'get-eshell)
(global-set-key (kbd "C-c E") #'myeshell)

(use-package eshell-z)

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(provide '30-eshell)
;;; 30-eshell.el ends here

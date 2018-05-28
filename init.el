;; ;; memo: display color and face names
;; (list-colors-display)
;; (list-faces-display)

;; Disable startup message.
(setq inhibit-startup-message t)

;; Set load-path.
(add-to-list 'load-path "~/lib/elisp")

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
;        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Initialize package system.
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Custom function to automatically install packages if not installed.
(defun install-package (package)
  (unless (package-installed-p package)
    (package-install package)))
(defun install-packages (package-list)
  (dolist (package package-list)
    (install-package package)))

;; use-package
(install-package 'use-package)
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; init-loader
(use-package init-loader
  :ensure t
  :config
  (setq init-loader-show-log-after-init 'error-only)
  (init-loader-load))

;; Custom set variables here.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" default))))

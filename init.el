(require 'package)
(require 'gnutls)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-user-dir (concat "~/.emacs.d/" emacs-version "/elpa"))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

(setq custom-file (concat "~/.emacs.d/" emacs-version "/custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'org)
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-init.org"))

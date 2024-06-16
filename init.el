;; Initial GUI configuration

(defvar my/default-font-family "Monospace")
(defvar my/default-font-size (cond
                              ((string= system-name "em780") 11)
                              (t 13)))

(set-face-attribute 'default nil
                    :foreground "#abb2bf"
                    :background "#282c34"
                    :family my/default-font-family
                    :height (* my/default-font-size 10))
(set-face-attribute 'mode-line nil
                    :foreground "#d3d3d3"
                    :background "#000000")
(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fringe-mode 10)
  (set-frame-size nil 100 35))

;; For measurement of startup time.

(defconst my/before-load-init-time (current-time))

;;;###autoload
(defun my/load-init-time ()
  "Loading time of user init files including time for `after-init-hook'."
  (let ((time1 (float-time
                (time-subtract after-init-time my/before-load-init-time)))
        (time2 (float-time
                (time-subtract (current-time) my/before-load-init-time))))
    (message (concat "Loading init files: %.0f [msec], "
                     "of which %.f [msec] for `after-init-hook'.")
             (* 1000 time1) (* 1000 (- time2 time1)))))
(add-hook 'after-init-hook #'my/load-init-time t)

(defvar my/tick-previous-time my/before-load-init-time)

;;;###autoload
(defun my/tick-init-time (msg)
  "Tick boot sequence at loading MSG."
  (when my/loading-profile-p
    (let ((ctime (current-time)))
      (message "---- %5.2f[ms] %s"
               (* 1000 (float-time
                        (time-subtract ctime my/tick-previous-time)))
               msg)
      (setq my/tick-previous-time ctime))))

(defun my/emacs-init-time ()
  "Emacs booting time in msec."
  (interactive)
  (message "Emacs booting time: %.0f [msec] = `emacs-init-time'."
           (* 1000
              (float-time (time-subtract
                           after-init-time
                           before-init-time)))))

(add-hook 'after-init-hook #'my/emacs-init-time)


;; Use straight.el for package management.

(setq straight-base-dir (concat "~/.emacs.d/" emacs-version))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Comment out package.el related settings.
;; (require 'package)
;; (require 'gnutls)
;; (setq package-archives
;;       '(("gnu" . "https://elpa.gnu.org/packages/")
;;         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")))
;; (setq package-user-dir (concat "~/.emacs.d/" emacs-version "/elpa"))
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (package-initialize)

(setq custom-file (concat "~/.emacs.d/" emacs-version "/custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(straight-use-package '(org :type built-in))
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-init.org"))

;; (profiler-report)
;; (profiler-stop)

;;; 35-ui-ext.el --- UI extension configuration
;;; Commentary:
;;; Code:

;; perspeen
(use-package perspeen
  :ensure t
  :init
  (setq perspeen-use-tab nil)
  :config
  (perspeen-mode)
  (define-key perspeen-command-map (kbd "o") 'perspeen-goto-last-ws)
  (define-key perspeen-command-map (kbd "C-p") 'perspeen-tab-prev)
  (define-key perspeen-command-map (kbd "C-n") 'perspeen-tab-next)
  (define-key perspeen-command-map (kbd "C-d") 'perspeen-tab-del))

;; popwin
;; (use-package popwin
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-c P") popwin:keymap)
;;   (setq popwin:popup-window-height 30)
;;   (push '(compilation-mode :noselect t :stick t :tail t :position bottom)
;;         popwin:special-display-config)
;;   ;; (push '("^\\*shell\\*" :regexp t :stick t :position bottom)
;;   ;;       popwin:special-display-config)
;;   (popwin-mode 1))

;; sr-speedbar
(use-package sr-speedbar
  :ensure t
  :bind (("C-c I s" . sr-speedbar-toggle))
  :config
  (setq sr-speedbar-right-side nil)
  (defadvice delete-other-windows (after my-sr-speedbar-delete-other-window-advice activate)
    "Check whether we are in speedbar, if it is, jump to next window."
    (let ()
      (when (and (sr-speedbar-window-exist-p sr-speedbar-window)
                 (eq sr-speedbar-window (selected-window)))
        (other-window 1)
        )))
  (ad-enable-advice 'delete-other-windows 'after 'my-sr-speedbar-delete-other-window-advice)
  (ad-activate 'delete-other-windows))

;; neotree
(use-package neotree
  :ensure t
  :bind (("C-c I n" . neotree-toggle)))

;; migemo
;; (install-package 'migemo)
;; (use-package migemo
;;   :config
;;   (setq migemo-command "cmigemo")
;;   (setq migemo-options '("-q" "--emacs"))
;;   (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
;;   (setq migemo-user-dictionary nil)
;;   (setq migemo-regex-dictionary nil)
;;   (setq migemo-coding-system 'utf-8-unix)
;;   (migemo-init))

;; Pin a window.
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message 
   (if (let (window (get-buffer-window (current-buffer)))
     (set-window-dedicated-p window 
                     (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))
(global-set-key (kbd "C-x L") 'toggle-window-dedicated)

(global-set-key (kbd "C-x O") '(lambda () (interactive) (other-window -1)))

;; ace-jump-mode
(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)))

;; ace-window
(use-package ace-window
  :ensure t
  :bind (("C-c 1" . aw-switch-to-window-1)
         ("C-c 2" . aw-switch-to-window-2)
         ("C-c 3" . aw-switch-to-window-3)
         ("C-c 4" . aw-switch-to-window-4)
         ("C-c 5" . aw-switch-to-window-5)
         ("C-c 6" . aw-switch-to-window-6)
         ("C-c 7" . aw-switch-to-window-7)
         ("C-c 8" . aw-switch-to-window-8)
         ("C-c 9" . aw-switch-to-window-9)
         ("C-c b" . ace-window)
         ("C-x x" . aw-flip-window)
         ("C-c x" . aw-flip-window))
  :config
  ;; generate aw-switch-to-window-N
  (require 'cl)
  (dotimes (num 9 t)
    (fset (intern (format "aw-switch-to-window-%d" (1+ num)))
          (lexical-let ((n num))
            (lambda () (interactive)
              (ignore-errors
                (aw-switch-to-window (nth n (aw-window-list))))))))

  (setq aw-background nil)
  (setq aw-scope 'frame)
  (ace-window-display-mode))

;; dim - change mode line names
(use-package dim
  :ensure t
  :config
  (dim-major-names
   '((emacs-lisp-mode "El")
     (lisp-interaction-mode "Li")
     (python-mode "Py")))
  (dim-minor-names
   '((eldoc-mode "" eldoc)
     (ws-butler-mode "" ws-butler)
     (smartparens-mode "" smartparens)
     (whitespace-mode "" whitespace)
     (yas-minor-mode "" yasnippet)
     (auto-revert-mode ""))))

;; smart-mode-line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

;; fancy-narrow
(use-package fancy-narrow
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fancy-narrow-mode))

;; emojify
(use-package emojify
  :ensure t)

(provide '35-ui-ext)
;;; 35-ui-ext.el ends here

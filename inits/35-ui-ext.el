;; perspeen
(use-package perspeen
  :ensure t
  :init
  (setq perspeen-use-tab t)
  :config
  (perspeen-mode)
  (define-key perspeen-command-map (kbd "o") 'perspeen-goto-last-ws)
  (define-key perspeen-command-map (kbd "C-p") 'perspeen-tab-prev)
  (define-key perspeen-command-map (kbd "C-n") 'perspeen-tab-next)
  (define-key perspeen-command-map (kbd "C-d") 'perspeen-tab-del))

;; popwin
(use-package popwin
  :ensure t
  :config
  (global-set-key (kbd "C-c P") popwin:keymap)
  (setq popwin:popup-window-height 30)
  (push '(compilation-mode :noselect t :stick t :tail t :position bottom)
        popwin:special-display-config)
  (push '("^\\*shell\\*" :regexp t :stick t :position bottom)
        popwin:special-display-config)
  (popwin-mode 1))

;; sr-speedbar
(use-package sr-speedbar
  :ensure t
  :bind (("C-c i s" . sr-speedbar-toggle))
  :config
  (setq sr-speedbar-right-side nil))

;; neotree
(use-package neotree
  :ensure t
  :bind (("C-c i n" . neotree-toggle)))

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
  :bind (("C-c b" . ace-window))
  :config
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

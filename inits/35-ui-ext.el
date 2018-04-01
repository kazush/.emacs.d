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

;; neotree
(use-package neotree
  :ensure t
  :bind (("C-c I n" . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

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

;; fancy-narrow
(use-package fancy-narrow
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fancy-narrow-mode))

;; emojify
(use-package emojify
  :ensure t)

;; transpose-frame
(use-package transpose-frame
  :ensure t
  :config
  (defhydra hydra-transpose-frame ()
    "transpose frame"
    ("x" transpose-frame)
    ("v" flip-frame)
    ("h" flop-frame)
    ("r" rotate-frame)
    ("j" rotate-frame-clockwise)
    ("k" rotate-frame-anticlockwise)
    ("q" nil)))


(provide '35-ui-ext)
;;; 35-ui-ext.el ends here

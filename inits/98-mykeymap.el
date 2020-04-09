;;; 98-mykeymap.el --- defines my keymap here
;;; Commentary:
;;; Code:

;; Define my keymap my-map and bind it to C-q.
(define-prefix-command 'my-map)
(global-set-key (kbd "C-q") 'my-map)
(define-key my-map (kbd "C-q") 'quoted-insert)

(define-key my-map (kbd "w") 'hydra-window/body)
(define-key my-map (kbd "r") 'hydra-rectangle/body)
(define-key my-map (kbd "t") 'hydra-transpose-frame/body)
(define-key my-map (kbd "g") 'hydra-git-gutter/body)
(define-key my-map (kbd "n") 'hydra-narrow/body)
(define-key my-map (kbd "m") 'hydra-multiple-cursors/body)

;; Define my keymap on "information/tips".
(define-prefix-command 'my-info-map)
(global-set-key (kbd "C-c i") 'my-info-map)

(define-key my-info-map (kbd "m") 'helm-semantic-or-imenu)
(define-key my-info-map (kbd "i") 'eldoc-box-eglot-help-at-point)

(when window-system
  (global-set-key (kbd "s-h") #'windmove-left)
  (global-set-key (kbd "s-j") #'windmove-down)
  (global-set-key (kbd "s-k") #'windmove-up)
  (global-set-key (kbd "s-l") #'windmove-right)
  (global-set-key (kbd "s-<tab>") #'switch-to-last-buffer)
  (global-set-key (kbd "s-n") #'switch-to-next-buffer)
  (global-set-key (kbd "s-p") #'switch-to-prev-buffer)
  (global-set-key (kbd "s-O") #'perspeen-goto-last-ws)
  (global-set-key (kbd "s-C") #'kill-buffer-and-window)
  (global-set-key (kbd "s-m") #'helm-mini)
  (global-set-key (kbd "s-f") #'find-file-other-window)
  (global-set-key (kbd "s-o") (lambda () (interactive)
                                (my/get-shell 1))))

(unless my/enable-helm
  (progn
    (global-set-key (kbd "C-x C-b") (lambda () (interactive)
                                      (ibuffer)))))

(provide '98-mykeymap)
;;; 98-mykeymap.el ends here

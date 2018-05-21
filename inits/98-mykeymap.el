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
(define-key my-map (kbd "f") 'hydra-flycheck/body)
(define-key my-map (kbd "n") 'hydra-narrow/body)
(define-key my-map (kbd "m") 'hydra-multiple-cursors/body)

(provide '98-mykeymap)
;;; 98-mykeymap.el ends here

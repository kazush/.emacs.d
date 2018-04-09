;;; 99-windows.el --- Configurations for window management
;;; Commentary:
;;; Code:

(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)
(setq window-combination-resize t)

;; To open a new window below the current buffer.
(add-to-list 'display-buffer-alist
             `(,(rx bos "*" (or "term" "shell" "eshell") (* not-newline) "*" eos)
               (lambda (buf alist)
                 (let ((win (get-buffer-window buf)))
                   (if win win
                     (display-buffer-in-side-window buf alist))))
               (side . bottom) (slot . -1) (preserve-size . (nil . t))
               (window-parameters . ((no-other-window . t) (no-delete-other-windows . t)))
               (window-height . 15)))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*" (or "Completion" "compilation" "helm") (* not-newline) "*" eos)
               (lambda (buf alist)
                 (let ((win (get-buffer-window buf)))
                   (if win win
                     (display-buffer-in-side-window buf alist))))
               (side . bottom) (slot . 1) (preserve-size . (nil . t))
               (window-parameters . ((no-other-window . t) (no-delete-other-windows . t)))
               (window-height . 15)))

(provide '99-mykeymap)
;;; 99-windows.el ends here

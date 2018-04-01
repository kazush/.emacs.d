;;; 99-windows.el --- Configurations for window management
;;; Commentary:
;;; Code:

;; To open a new window below the current buffer.
(add-to-list 'display-buffer-alist
             `(,(rx bos "*" (or "helm" "compilation" "term" "shell" "eshell") (* not-newline) "*" eos)
               (lambda (buf alist)
                 (let ((win (get-buffer-window buf)))
                   (if win win
                     (display-buffer-in-atom-window buf alist))))
               (inhibit-same-window . t)
               (window-height . 0.3)))

(provide '99-mykeymap)
;;; 99-windows.el ends here

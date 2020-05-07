;;; 98-windows.el --- Configurations for window management
;;; Commentary:
;;; Code:

(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)
(setq window-combination-resize t)

(defvar my/dba-min-windows 3)
(defun my/display-buffer-action (buf alist)
  "Return a window to display buffer BUF.  ALIST is not used."
  (let ((win (get-buffer-window buf))
        (buflist (reverse (buffer-list (selected-frame)))))
    (if win win
      (setq win (get-buffer-window "*scratch*"))
      (unless (or win (< (count-windows) my/dba-min-windows))
        (while buflist
          (let* ((b (car buflist))
                 (w (get-buffer-window b)))
            (if (or (eq b (current-buffer))
                    (null w)
                    (not (window-live-p w))
                    (window-minibuffer-p w)
                    (window-dedicated-p w)
                    (seq-contains '(exwm-mode shell-mode eshell-mode term-mode)
                                  (with-current-buffer b major-mode)))
                (setq buflist (cdr buflist))
              (setq win w)
              (setq buflist nil))))))
    (if win
        (set-window-buffer win buf))
    win))

(setq display-buffer-fallback-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         ;; display-buffer-reuse-mode-window
         my/display-buffer-action
         display-buffer--maybe-pop-up-frame-or-window
         display-buffer-in-previous-window
         display-buffer-use-some-window
         display-buffer-pop-up-frame)))

;; Newer version of helm does not need this workaround.
;; (define-advice helm-persistent-action-display-window
;;     (:around (orig-fn &optional split-window) "always-no-split")
;;   ;; (message "always-no-split called")
;;   (let ((w (get-buffer-window helm-buffer)))
;;     (if (window-dedicated-p w)
;;         w
;;       (orig-fn split-window))))

(defvar my/side-window-height .4)

;; To open a new window below the current buffer.
(add-to-list 'display-buffer-alist
             `(,(rx bos "*" (or "term" "shell" "eshell") (* not-newline) "*" eos)
               (lambda (buf alist)
                 (let ((win (get-buffer-window buf)))
                   (if win win
                     (display-buffer-in-side-window buf alist))))
               (side . bottom) (slot . -1) (preserve-size . (nil . t))
               (window-parameters . ((no-other-window . t) (no-delete-other-windows . t)))
               (window-height . ,my/side-window-height)))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*"
                    (or "Completion" "compilation" "helm" "Buffer List" (regexp "build.*") "xref")
                    (* not-newline) "*" eos)
               (lambda (buf alist)
                 (let ((win (get-buffer-window buf)))
                   (if win win
                     (display-buffer-in-side-window buf alist))))
               (side . bottom) (slot . 1) (preserve-size . (nil . t))
               (window-parameters . ((no-other-window . t) (no-delete-other-windows . t)))
               (window-height . ,my/side-window-height)))

(defun my/display-buffer-in-bottom-window (bufname slot)
  "Display buffer with name BUFNAME in a window with slot SLOT at the bottom."
  (display-buffer-in-side-window (get-buffer-create bufname)
                                 `((side . bottom) (slot . ,slot))))
(global-set-key (kbd "C-c w l")
                (lambda (bufname)
                  (interactive "B")
                  (my/display-buffer-in-bottom-window bufname -1)))
(global-set-key (kbd "C-c w c")
                (lambda (bufname)
                  (interactive "B")
                  (my/display-buffer-in-bottom-window bufname 0)))
(global-set-key (kbd "C-c w r")
                (lambda (bufname)
                  (interactive "B")
                  (my/display-buffer-in-bottom-window bufname 1)))

(provide '98-windows)
;;; 98-windows.el ends here

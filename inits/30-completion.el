
(use-package ycmd
  :ensure t
  :config
  (set-variable 'ycmd-server-command
                (list "python"
                      (substitute-in-file-name "$HOME/ws/ycmd/ycmd/__main__.py")))
  (setq request-message-level -1)
  (setq url-show-status nil)
  (add-hook 'after-init-hook #'global-ycmd-mode))

(use-package company
  :ensure t
  :config
  (setq company-show-numbers t)
  (setq company-lighter "")
  (setq company-idle-delay 0.2)
  (setq company-echo-delay 0)
  (setq company-dabbrev-downcase nil)
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'eshell-mode-hook #'company-mode)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (require 'color)
  (let ((bg (face-attribute 'default :background)))
    (set-face-attribute 'company-tooltip nil
                        :inherit 'default
                        :background (color-lighten-name bg 20))
    (set-face-attribute 'company-scrollbar-bg nil
                        :background (color-lighten-name bg 20))
    (set-face-attribute 'company-scrollbar-fg nil
                        :background (color-lighten-name bg 10))
    (set-face-attribute 'company-tooltip-selection nil
                        :inherit 'company-tooltip)
    (set-face-attribute 'company-tooltip-common-selection nil
                        :inherit 'company-tooltip)
    (set-face-attribute 'company-tooltip-common nil
                        :inherit 'company-tooltip
                        :background (color-lighten-name bg 20))
    )
  ;; quoted from https://oremacs.com/2017/12/27/company-numbers/
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    (define-key map (kbd "<return>") nil))
  (defun ora-company-number ()
    "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
        (company-complete-number (string-to-number k)))))
  )

(use-package company-ycmd
  :ensure t
  :config
  (company-ycmd-setup)
  (setq company-ycmd-request-sync-timeout 0))

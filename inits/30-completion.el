(use-package company
  :hook ((prog-mode eshell-mode) . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-show-numbers t)
  (setq company-lighter "")
  (setq company-idle-delay 0.2)
  (add-hook 'shell-mode-hook (lambda () (setq-local company-idle-delay 0.5)))
  (add-hook 'eshell-mode-hook (lambda () (setq-local company-idle-delay 0.5)))
  (setq company-echo-delay 0)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t)
  (defun my/set-company-tooltip-faces ()
    "Set faces for company tooltip."
    (interactive)
    (let ((bg (face-attribute 'default :background)))
      (set-face-attribute 'company-tooltip nil
                          :background (color-lighten-name
                                       (face-background 'default)
                                       10)
                          :inherit 'default)
      (set-face-attribute 'company-tooltip-selection nil
                          :foreground 'unspecified
                          :background (color-lighten-name
                                       (face-background 'company-tooltip)
                                       10)
                          :inherit 'company-tooltip)
      (set-face-attribute 'company-tooltip-common nil
                          :background 'unspecified
                          :inherit 'company-tooltip)
      (set-face-attribute 'company-tooltip-common-selection nil
                          :foreground 'unspecified
                          :background (face-background
                                       'company-tooltip-selection)
                          :inherit 'company-tooltip-common)
      (set-face-attribute 'company-tooltip-annotation nil
                          :foreground "LightSlateBlue"
                          :background 'unspecified
                          :inherit 'company-tooltip)
      (set-face-attribute 'company-tooltip-annotation-selection nil
                          :foreground 'unspecified
                          :background (face-background
                                       'company-tooltip-selection)
                          :inherit 'company-tooltip-annotation)
      (set-face-attribute 'company-scrollbar-bg nil
                          :background (color-lighten-name bg 10))
      (set-face-attribute 'company-scrollbar-fg nil
                          :background (color-lighten-name bg 40))
      (set-face-attribute 'company-template-field nil
                          :foreground (face-foreground
                                       'company-tooltip-annotation)
                          :background 'unspecified
                          :slant 'unspecified
                          :inherit 'default)
      ))
  (add-hook 'emacs-startup-hook 'my/set-company-tooltip-faces)
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

(use-package company-quickhelp
  :after (company)
  :config
  (customize-set-variable 'company-quickhelp-delay nil)
  (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
  (company-quickhelp-mode))

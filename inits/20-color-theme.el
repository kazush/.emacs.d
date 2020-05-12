;; Workaround to make atom-one-dark theme work on tty
;; (defun tty-color-approximate (rgb &optional frame)
;;   "Find the color in `tty-color-alist' that best approximates RGB.
;; Value is a list of the form (NAME INDEX R G B).
;; The argument RGB should be an rgb value, that is, a list of three
;; integers in the 0..65535 range.
;; FRAME defaults to the selected frame."
;;   (let* ((color-list (tty-color-alist frame))
;;          (candidate (car color-list))
;;          (best-distance 195076) ;; 3 * 255^2 + 15
;;          (r (ash (car rgb) -8))
;;          (g (ash (cadr rgb) -8))
;;          (b (ash (nth 2 rgb) -8))
;;          best-color)
;;     (while candidate
;;       (let ((try-rgb (cddr candidate))
;;             ;; If the approximated color is not close enough to the
;;             ;; gray diagonal of the RGB cube, favor non-gray colors.
;;             ;; (The number 0.065 is an empirical ad-hoc'ery.)
;;             (favor-non-gray (>= (tty-color-off-gray-diag r g b) 0.065))
;;             try-r try-g try-b
;;             dif-r dif-g dif-b dist)
;;         ;; If the RGB values of the candidate color are unknown, we
;;         ;; never consider it for approximating another color.
;;         (if try-rgb
;;             (progn
;;               (setq try-r (lsh (car try-rgb) -8)
;;                     try-g (lsh (cadr try-rgb) -8)
;;                     try-b (lsh (nth 2 try-rgb) -8))
;;               (setq dif-r (- r try-r)
;;                     dif-g (- g try-g)
;;                     dif-b (- b try-b))
;;               (setq dist (+ (* dif-r dif-r) (* dif-g dif-g) (* dif-b dif-b)))
;;               (if (and (< dist best-distance)
;;                        ;; Comment out to make some dark themes look better
;;                        ;; ;; The candidate color is on the gray diagonal
;;                        ;; if its RGB components are all equal.
;;                        ;; (or (/= try-r try-g) (/= try-g try-b)
;;                        ;;     (not favor-non-gray)))
;;                        t)
;;                   (setq best-distance dist
;;                         best-color candidate)))))
;;       (setq color-list (cdr color-list))
;;       (setq candidate (car color-list)))
;;     best-color))

(use-package zerodark-theme
  :config
  (load-theme 'zerodark t)
  ;; (zerodark-setup-modeline-format)
  )

(defun my/set-default-faces ()
  "Set default faces."
  (interactive)
  (set-face-attribute 'default nil
                      :background "gray15")
  (set-face-attribute 'region nil
                      :foreground 'unspecified
                      :background "gray30")
  ;; (set-face-attribute 'font-lock-comment-face nil
  ;;                     :foreground "LightSlateGray"
  ;;                     :slant 'italic
  ;;                     :inherit 'default)
  ;; (set-face-attribute 'font-lock-comment-delimiter-face nil
  ;;                     :slant 'unspecified
  ;;                     :inherit 'font-lock-comment-face)
  (set-face-attribute 'highlight nil
                      :foreground "orange"
                      :background 'unspecified
                      :weight 'bold
                      :inherit 'default)
  (set-face-foreground 'font-lock-variable-name-face "khaki")
  (unless (daemonp)
    (require 'color)
    (set-face-attribute 'mode-line-inactive nil
                        :foreground (face-foreground 'mode-line)
                        :background (color-darken-name
                                     (face-background 'mode-line) 10)
                        :inherit 'mode-line)
    (set-face-foreground 'font-lock-function-name-face
                         (color-darken-name
                          (face-foreground 'font-lock-type-face) 10))
    (set-face-foreground 'font-lock-preprocessor-face
                         (color-lighten-name
                          (face-foreground 'font-lock-keyword-face) 10)))
  )

;; ;; Set face for hl-line-mode
;; (set-face-background hl-line-face "gray22")
;; (set-face-foreground hl-line-face nil)

(use-package all-the-icons
  :config
  (setq inhibit-compacting-font-caches t))

(use-package spaceline
  :config
  (defun my/setup-spaceline ()
    (if (not window-system)
        (setq powerline-default-separator 'utf-8))
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)
    ;; (spaceline-helm-mode)
    (spaceline-info-mode)))

(defun my/setup-ui-theme ()
  "Sets up UI theme."
  (interactive)
  (if (daemonp)
      (load-theme 'zerodark t))
  (my/setup-spaceline)
  (my/set-default-faces))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (my/setup-ui-theme)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (my/setup-ui-theme))))

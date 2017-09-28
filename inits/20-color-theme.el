;; Workaround to make atom-one-dark theme work on tty
(defun tty-color-approximate (rgb &optional frame)
  "Find the color in `tty-color-alist' that best approximates RGB.
Value is a list of the form (NAME INDEX R G B).
The argument RGB should be an rgb value, that is, a list of three
integers in the 0..65535 range.
FRAME defaults to the selected frame."
  (let* ((color-list (tty-color-alist frame))
         (candidate (car color-list))
         (best-distance 195076) ;; 3 * 255^2 + 15
         (r (ash (car rgb) -8))
         (g (ash (cadr rgb) -8))
         (b (ash (nth 2 rgb) -8))
         best-color)
    (while candidate
      (let ((try-rgb (cddr candidate))
            ;; If the approximated color is not close enough to the
            ;; gray diagonal of the RGB cube, favor non-gray colors.
            ;; (The number 0.065 is an empirical ad-hoc'ery.)
            (favor-non-gray (>= (tty-color-off-gray-diag r g b) 0.065))
            try-r try-g try-b
            dif-r dif-g dif-b dist)
        ;; If the RGB values of the candidate color are unknown, we
        ;; never consider it for approximating another color.
        (if try-rgb
            (progn
              (setq try-r (lsh (car try-rgb) -8)
                    try-g (lsh (cadr try-rgb) -8)
                    try-b (lsh (nth 2 try-rgb) -8))
              (setq dif-r (- r try-r)
                    dif-g (- g try-g)
                    dif-b (- b try-b))
              (setq dist (+ (* dif-r dif-r) (* dif-g dif-g) (* dif-b dif-b)))
              (if (and (< dist best-distance)
                       ;; Comment out to make some dark themes look better
                       ;; ;; The candidate color is on the gray diagonal
                       ;; if its RGB components are all equal.
                       ;; (or (/= try-r try-g) (/= try-g try-b)
                       ;;     (not favor-non-gray)))
                       t)
                  (setq best-distance dist
                        best-color candidate)))))
      (setq color-list (cdr color-list))
      (setq candidate (car color-list)))
    best-color))

(add-hook 'font-lock-mode-hook
          '(lambda ()
             ;; (set-face-background 'mode-line "gray20")
             ;; (set-face-background 'default "gray15")
             (set-face-foreground 'region nil)
             (set-face-background 'region "navy")
             ;; (set-face-background 'highlight "gray30")
             ;; (set-face-foreground 'highlight nil)
             (set-face-foreground 'font-lock-type-face "PaleGoldenrod")
             )
          'APPEND)

;; ;; Set face for hl-line-mode
(set-face-background hl-line-face "gray22")
(set-face-foreground hl-line-face nil)

;; ;; Use zenburn theme.
;; (use-package hc-zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'hc-zenburn t))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

;; (setq color-themes '())
;; (use-package color-theme-solarized
;;    :ensure t
;;    :config
;;    (setq frame-background-mode 'dark)
;;    (setq solarized-termcolors 16)
;;    (load-theme 'solarized t))

;; (use-package atom-one-dark-theme
;;   :ensure t
;;   :config
;;   (load-theme 'atom-one-dark t))

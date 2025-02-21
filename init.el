;; Initial GUI configuration

(defvar my/default-font "Monospace")
(defvar my/default-font-size 16)  ;; pixel size not point
(defvar my/default-jp-font "Noto Sans CJK JP")
(defvar my/default-emoji-font "Noto Color Emoji")
(defvar my/font-rescale-alist
  '(("Noto Color Emoji" . ((8 . 0.88)
                           (9 . 0.92)
                           (16 . 0.86)
                           ;; (27 . 0.82)
                           (32 . 0.84)))
    ("Noto Sans CJK JP" . ((8 . 1.1)
                           (9 . 1.2)
                           (11 . 1.1)
                           (16 . 1.053)))))

(defun my/get-font-rescale-param (font font-size)
  "Get font rescale params suitable for FONT-SIZE."
  (require 'cl-lib)
  (let ((rescale-param-alist
         (cl-loop for ent in my/font-rescale-alist
                  when (string-match (car ent) font) return (cdr ent))))
    (when rescale-param-alist
      (let ((scale
            (cl-loop for pair in rescale-param-alist
                     when (<= (car pair) font-size)
                     maximize (car pair) into max-key
                     finally return (cdr (assoc max-key rescale-param-alist)))))
        ;; fallback to the first element.
        (if scale scale (cdar rescale-param-alist))))))

(defun my/update-font-rescale-params (inc)
  "Adjust rescale parameters for JP and Emoji fonts."
  (let ((new-font-size (if (zerop inc) my/default-font-size
                         (round (* my/default-font-size (expt text-scale-mode-step
                                                              text-scale-mode-amount)))
                         ;; (font-get (face-attribute 'default ':font) ':size)
                         )))
    (dolist (font (list my/default-jp-font my/default-emoji-font))
      (let ((scale (my/get-font-rescale-param font new-font-size)))
        ;; for debugging only
        ;; (message "font=%s newsize:%d scale:%s" font new-font-size scale)
        (when scale
          (setf (alist-get font face-font-rescale-alist nil nil 'equal)
                scale))))))

(advice-add 'text-scale-increase :after #'my/update-font-rescale-params)

(defun my/make-font-str (font &optional size)
  "Make font string which can be used for :font in set-face-attribute."
  (let ((font-str (format "%s%s" font
                          (if size (format ":size=%d" size) ""))))
    (x-resolve-font-name
     (format "%s:weight=regular:slant=normal" font-str))))

(defun my/set-default-font-size ()
  "Set default font size for each font as per the current monitor environment."
  (interactive)
  (setq my/default-font-size
        (pcase (format "%s:%s"
                       (alist-get 'name (frame-monitor-attributes))
                       system-name )
          ((rx "DELL S2722") 16)
          ((rx (: (1+ anychar) ":minibookx" eos)) 19)
          (_ 16)))

  ;; Update face-font-rescale-alist with default font size.
  (my/update-font-rescale-params 0)

  ;; A new fontset named "fontset-auto1" is created and set to the default face.
  ;; The same fontset should be used in the subsequent font settings of default
  ;; face, otherwise another new auto fontset will be created in each call.
  ;; To get the fontset for default face, use (face-attribute 'default :fontset).
  (set-face-attribute 'default nil
                      :font (my/make-font-str my/default-font
                                              my/default-font-size)))


;; Initialize default font.
(if (display-graphic-p)
    (my/set-default-font-size))

;; Use straight.el for package management.

(setq straight-base-dir (concat "~/.emacs.d/" emacs-version))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Comment out package.el related settings.
;; (require 'package)
;; (require 'gnutls)
;; (setq package-archives
;;       '(("gnu" . "https://elpa.gnu.org/packages/")
;;         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
;;         ("melpa" . "https://melpa.org/packages/")))
;; (setq package-user-dir (concat "~/.emacs.d/" emacs-version "/elpa"))
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;; (package-initialize)

(setq custom-file (concat "~/.emacs.d/" emacs-version "/custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(straight-use-package '(org :type built-in))
(org-babel-load-file (expand-file-name "~/.emacs.d/emacs-init.org"))

;; (profiler-report)
;; (profiler-stop)

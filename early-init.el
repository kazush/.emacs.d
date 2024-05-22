;; (require 'profiler)
;; (profiler-start 'cpu)

;; GC settings.
(setq gc-cons-threshold (* 100 1024 1024))
(setq garbage-collection-messages t)

;; For straight.
(setq package-enable-at-startup nil
      straight-check-for-modifications '(check-on-save find-when-checking)
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      ;; inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

;; GUI settings.
(mapc
 (lambda (frame-alist)
   (push '(menu-bar-lines . 0) frame-alist)
   (push '(tool-bar-lines . 0) frame-alist)
   (push '(foreground-color . "#abb2bf") frame-alist)
   (push '(background-color . "#282c34") frame-alist)
   (push '(vertical-scroll-bars) frame-alist))
 '(default-frame-alist initial-frame-alist))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(fringe-mode 5)

(setq mode-line-format nil)
(set-face-attribute 'default nil :foreground "#282c34" :background "#282c34")
(set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)

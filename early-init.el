
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
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

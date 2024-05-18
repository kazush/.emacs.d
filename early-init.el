;; For straight.
(setq package-enable-at-startup nil)
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; UI settings.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(setq frame-inhibit-implied-resize t)

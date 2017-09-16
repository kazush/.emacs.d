;; Load local init files if exists.
(when (file-directory-p "~/.emacs.d/local/preinit")
  (init-loader-load "~/.emacs.d/local/preinit"))

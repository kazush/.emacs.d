;; Load local init files if exists.
(when (file-directory-p "~/.emacs.d/local/postinit")
  (init-loader-load "~/.emacs.d/local/postinit"))

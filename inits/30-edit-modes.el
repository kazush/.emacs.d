;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml$" . yaml-mode))
  :init
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; extended conf modes
(require 'generic-x)

;; markdown
;; gfm-preview is a 1-line script containing "grip --export $1 -"
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md$" . gfm-mode)
         ("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode))
  :init
  (setq markdown-command "gfm-preview"))

;; plantuml
(use-package plantuml-mode
  :ensure t
  :mode (("\\.uml$" . plantuml-mode))
  :config
  (with-eval-after-load 'org
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))
  (setq plantuml-jar-path "~/Downloads/plantuml.jar"))

(setq org-plantuml-jar-path "~/Downloads/plantuml.jar")
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t))))

;; graphviz-dot
(use-package graphviz-dot-mode
  :ensure t
  :mode (("\\.dot$" . graphviz-dot-mode)))
